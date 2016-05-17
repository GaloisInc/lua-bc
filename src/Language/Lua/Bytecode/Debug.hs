{-# LANGUAGE RecordWildCards #-}
module Language.Lua.Bytecode.Debug where

import Data.ByteString (ByteString, append)
import qualified Data.Vector as Vector
import           Data.String(fromString)
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)

import Language.Lua.Bytecode
import Language.Lua.Bytecode.FunId



------------------------------------------------------------------------
-- Debugging functions
------------------------------------------------------------------------

lookupLineNumber ::
  Function ->
  Int {- ^ program counter -} ->
  Maybe Int
lookupLineNumber Function{..} pc = debugInfoLines Vector.!? pc
  where
  DebugInfo{..} = funcDebug


{- | Given a function, compute a map from line numbers to
op-codes in the function.  This is useful for adding break-points
identified by line number. Does not consider nested functions. -}
shallowLineNumberMap :: Function -> Map Int [Int]
shallowLineNumberMap f = Map.fromListWith (++)
                                        (mapMaybe lookupPc [ 0 .. ops - 1 ])
  where
  ops        = Vector.length (funcCode f)
  lookupPc pc = do ln <- lookupLineNumber f pc
                   return (ln,[pc])

{- | Given a function, compute a map from line numbers to op-codes
in this function or a nested function.  For each line number we
return a list of pairs (typically just 1). The first element in the
pair is the path to the nested function---empty if not nested---and
the second one are the PC locations associated with that function. -}
deepLineNumberMap :: Function -> Map Int [ (FunId, [Int]) ]
deepLineNumberMap f0 = Map.unionsWith (++)
                     $ me : zipWith child [ 0 .. ]
                              (Vector.toList (funcProtos f0))
  where
  me              = fmap (\pc -> [ (noFun, pc) ]) (shallowLineNumberMap f0)
  ext n (path,pc) = (subFun path n,pc)
  child n f       = fmap (map (ext n)) (deepLineNumberMap f)





-- | Compute the locals at a specific program counter.
-- This function is memoized, so it is fast to lookup things many times.
lookupLocalName :: Function -> Int -> Reg -> Maybe ByteString
lookupLocalName func = \pc (Reg x) -> do vs <- memo Vector.!? pc
                                         vs Vector.!? x
  where
  memo      = Vector.generate (Vector.length (funcCode func)) locals
  locals pc = Vector.map varInfoName
            $ Vector.filter (\x -> pc < varInfoEnd x)
            $ Vector.takeWhile (\x -> varInfoStart x <= pc)
            $ debugInfoVars (funcDebug func)




-- | Compute the names for the functions defined withing the given function.
-- The 'Int' is the index of the sub-function's prototype.
inferSubFunctionNames :: Function -> [ (Int, ByteString) ]
inferSubFunctionNames fun = foldr checkOp [] (Vector.indexed (funcCode fun))
  where
  checkOp (pc,op) rest =
    case op of
      OP_CLOSURE _ (ProtoIx k)
        | Just nm <- inferFunctionName fun pc -> (k,nm) : rest
      _ -> rest




-- | Figure out a name for the function defined at the given program
-- counter in a function.  Note that this operation could be fairly expensive,
-- so probably a good idea to cache the results.
inferFunctionName :: Function -> Int -> Maybe ByteString
inferFunctionName fun pc0 =
  do op <- getOp pc0
     case op of
       OP_CLOSURE r (ProtoIx _) -> findForward r (pc0 + 1)
       _                        -> Nothing
  where
  getOp pc          = funcCode fun Vector.!? pc
  getLoc            = lookupLocalName fun
  getUp (UpIx n)    = debugInfoUpvalues (funcDebug fun) Vector.!? n
  getLab k          = case k of
                        RK_Kst (Kst n) ->
                          do v <- funcConstants fun Vector.!? n
                             case v of
                               KString lab -> return lab
                               _           -> Nothing
                        RK_Reg _ -> Nothing

  jn x y            = append x (append (fromString ".") y)


  -- Look forward for an instraction that puts a given register in a table.
  findForward r pc =
    case getLoc pc r of
      Just x  -> return x
      Nothing ->
        do op <- getOp pc
           case op of
             OP_MOVE r1 r2
               | r == r2  -> findForward r1 (pc + 1)

             OP_SETTABLE r1 k (RK_Reg r2)
               | r2 == r -> do nm  <- getNameFor r1 pc
                               lab <- getLab k
                               return (jn nm lab)

             OP_SETUPVAL r2 u
                | r2 == r -> getUp u

             OP_SETTABUP u k (RK_Reg r2)
                | r2 == r -> do nm  <- getUp u
                                lab <- getLab k
                                return (jn nm lab)

             OP_CALL r2 _ _
               | r2 <= r -> return (fromString "_")

             OP_TAILCALL r2 _ _
               | r2 <= r -> return (fromString "_")

             _ -> findForward r (pc + 1)

  findBack r pc =
    do op <- getOp pc
       case op of

         OP_GETTABLE r1 r2 k
            | r1 == r -> do nm  <- getNameFor r2 pc
                            lab <- getLab k
                            return (jn nm lab)

         OP_GETUPVAL r1 u
            | r1 == r  -> getUp u

         OP_GETTABUP r1 u k
            | r1 == r  -> do nm  <- getUp u
                             lab <- getLab k
                             return (jn nm lab)

         OP_NEWTABLE r1 _ _
            | r1 == r -> findForward r (pc + 1)

         _ -> findBack r (pc - 1)

  getNameFor r pc =
    case getLoc pc r of
      Just x  -> return x
      Nothing -> findBack r (pc - 1)



