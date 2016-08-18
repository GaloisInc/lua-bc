{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Language.Lua.Bytecode.Debug where

import Data.ByteString (ByteString,append)
import qualified Data.ByteString.Char8 as BS
import           Data.Vector (Vector)
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
  locals pc = Vector.map varInfoName $ getRegistersAt func pc

-- | Get what registers are in scope at a particular op-code.
-- R1 is at entry 0, R2 is entry 1, etc.
-- NOTE that there might be multiple registers with the same named thing.
-- The one currently in scope is the last one.
getRegistersAt :: Function -> Int {-^PC-} -> Vector VarInfo
getRegistersAt func pc = Vector.filter (\x -> pc < varInfoEnd x)
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
                               KString lab -> return (append "." lab)
                               KInt x      -> return (numIx x)
                               KNum x      -> return (numIx x)
                               _           -> Nothing
                        RK_Reg _ -> Nothing

  numIx x           = BS.concat [ "[", BS.pack (show x), "]" ]


  -- Look forward for an instruction that puts a given register in place with a known name.
  findForward r pc =
    case getLoc pc r of
      Just x  -> return x
      Nothing ->
        do op <- getOp pc
           case op of

             -- Placed in a register
             OP_MOVE r1 r2
               | r == r2  -> findForward r1 (pc + 1)

             -- Placed in a table
             OP_SETTABLE r1 k (RK_Reg r2)
               | r2 == r -> do nm  <- getNameFor r1 pc
                               lab <- getLab k
                               return (append nm lab)

             -- Placed in an up-value
             OP_SETUPVAL r2 u
                | r2 == r -> getUp u

             -- Placed in an up-value table
             OP_SETTABUP u k (RK_Reg r2)
                | r2 == r -> do nm  <- getUp u
                                lab <- getLab k
                                return (append nm lab)

             -- Placed in a table, many indices at once
             OP_SETLIST t howMany from
                | t < r && (howMany == 0 || r <= plusReg t howMany) ->
                  do nm <- getNameFor t pc
                     start <-
                       if from == 0
                          then do OP_EXTRAARG s <- getOp (pc + 1)
                                  return s
                          else return ((from - 1) * 50)
                     return (append nm (numIx (start + (diffReg r t))))

             -- Passed as an argument to a function
             OP_CALL r2 _ _
               | r2 <= r -> return (fromString "_")

             -- Passed as an argument to a function
             OP_TAILCALL r2 _ _
               | r2 <= r -> return (fromString "_")

             _ -> findForward r (pc + 1)

  -- Find the name of a register
  findBack r pc =
    do op <- getOp pc
       case op of

         OP_GETTABLE r1 r2 k
            | r1 == r -> do nm  <- getNameFor r2 pc
                            lab <- getLab k
                            return (append nm lab)

         OP_GETUPVAL r1 u
            | r1 == r  -> getUp u

         OP_GETTABUP r1 u k
            | r1 == r  -> do nm  <- getUp u
                             lab <- getLab k
                             return (append nm lab)

         OP_NEWTABLE r1 _ _
            | r1 == r -> findForward r (pc + 1)

         _ -> findBack r (pc - 1)

  getNameFor r pc =
    case getLoc pc r of
      Just x  -> return x
      Nothing -> findBack r (pc - 1)



