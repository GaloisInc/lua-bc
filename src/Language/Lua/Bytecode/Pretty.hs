module Language.Lua.Bytecode.Pretty where

import Language.Lua.Bytecode
import Language.Lua.Bytecode.Debug(lookupLocalName)
import Language.Lua.Bytecode.FunId

import Text.PrettyPrint
import Data.Maybe(fromMaybe)
import Data.Text.Lazy()
import Data.Text.Encoding(decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as Vector
import Numeric (showHex)

data PPInfo = PPInfo
  { ppVarAt    :: Int -> Reg  -> Maybe Doc
  , ppConstant :: Kst         -> Maybe Doc
  , ppUpIx     :: UpIx        -> Maybe Doc
  , ppFun      :: ProtoIx     -> Maybe Doc
  , ppExtraArg :: Maybe Int
  , ppPC       :: !Int
  }

ppVar :: PPInfo -> Reg -> Maybe Doc
ppVar p = ppVarAt p (ppPC p)

blankPPInfo :: PPInfo
blankPPInfo = PPInfo
  { ppVarAt    = \_ _ -> Nothing
  , ppConstant = \_   -> Nothing
  , ppUpIx     = \_   -> Nothing
  , ppFun      = \_   -> Nothing
  , ppExtraArg = Nothing
  , ppPC       = 0
  }

ppNextPC :: PPInfo -> PPInfo
ppNextPC p = p { ppPC = 1 + ppPC p }

ppCode :: Function -> Doc
ppCode f = vcat $ map (ppOpCode f) [ 0 .. Vector.length (funcCode f) - 1 ]

ppOpCode :: Function -> Int -> Doc
ppOpCode f pc0 = case funcCode f Vector.!? pc0 of
                   Just op -> pp us op
                   Nothing -> parens (text ("op code at " ++ show pc0))
  where
  us = PPInfo { ppVarAt     = lkpVar
              , ppConstant  = lkpConstant
              , ppUpIx      = lkpUpIx
              , ppFun       = lkpFun
              , ppExtraArg  = lkpExtra
              , ppPC        = pc0
              }

  -- This is written like this so that `lkp` memoizes.
  lkpVar = \pc r -> do bs <- lkp pc r
                       return (text (BS.unpack bs))
    where lkp = lookupLocalName f

  lkpUpIx (UpIx u) =
    do bs <- debugInfoUpvalues (funcDebug f) Vector.!? u
       return (text (BS.unpack bs))

  lkpConstant (Kst k) = fmap (pp us) (funcConstants f Vector.!? k)

  lkpExtra = do OP_EXTRAARG a <- funcCode f Vector.!? (pc0 + 1)
                return a

  lkpFun (ProtoIx x) =
    do f1 <- funcProtos f Vector.!? x
       let nm = case funcSource f1 of
                  Just bs -> "function from " ++ BS.unpack bs
                  Nothing -> "function at"
       return (text nm <+> parens (int (funcLineDefined f1) <> text "-" <>
                                   int (funcLastLineDefined f1)))



instance PP Constant where
  pp _ cnst =
    case cnst of
      KNil          -> text "nil"
      KBool b       -> text (if b then "true" else "false")
      KNum d        -> double d
      KInt n        -> int n
      KString bs    -> str bs
      KLongString b -> str b

    where str = text . showLuaString

showLuaString :: BS.ByteString -> String
showLuaString bs = '"' : BS.foldr show1 "\"" bs
  where
    show1 x
      | x == '"'                = showString "\\\""
      | x == '\\'               = showString "\\\\"
      | '\x20' <= x, x < '\x7f' = showChar x
      | '\x10' <= x             = showString "\\x"  . showHex (fromEnum x)
      | otherwise               = showString "\\x0" . showHex (fromEnum x)


class PP a where
  pp :: PPInfo -> a -> Doc

instance PP FunId where
  pp n = text . funIdString

instance PP Reg where
  pp i r@(Reg x) = fromMaybe d (ppVar i r)
    where d = text "R" <> brackets (int (x+1))

instance PP Kst where
  pp i k@(Kst x) = fromMaybe d (ppConstant i k)
    where d = text "K" <> brackets (int (x+1))

instance PP UpIx where
  pp i u@(UpIx x) = fromMaybe d (ppUpIx i u)
    where d = text "U" <> brackets (int (x+1))

instance PP ProtoIx where
  pp i p@(ProtoIx x) = fromMaybe d (ppFun i p)
    where d = text "KPROTO" <> brackets (int (x+1))

instance PP RK where
  pp i rk =
    case rk of
      RK_Reg r -> pp i r
      RK_Kst k -> pp i k

instance PP Doc where
  pp _ d = d

instance PP Bool where
  pp _ b = text (if b then "true" else "false")

ppRegRange :: PPInfo -> Reg -> Int -> Doc
ppRegRange _ _ 0 = empty
ppRegRange i r 1 = pp i r
ppRegRange i r@(Reg x) n =
  case ppVar i r of
    Just v  -> v <> comma <+> ppRegRange i (succ r) (n-1)
    Nothing -> pp i r <+> text ".." <+> pp i (Reg (x + n - 1))


ppRegRangeInf :: PPInfo -> Reg -> Doc
ppRegRangeInf i r0 = hsep (punctuate comma (ppE r0)) <> text ".."
  where
  ppE r@(Reg x) = case ppVar i r of
                    Just v  -> v : ppE (Reg (x+1))
                    Nothing -> [ pp i r ]

ppRegRangeCount :: PPInfo -> Reg -> Count -> Doc
ppRegRangeCount i r ct =
  case ct of
    CountTop   -> ppRegRangeInf i r
    CountInt n -> ppRegRange i r n





instance PP OpCode where
  pp i opCode =
    let (=:) :: (PP a, PP b) => a -> b -> Doc
        x =: y = let d1 = pp (ppNextPC i) x
                     d2 = pp i y
                 in if isEmpty d1 then d2 else d1 <+> text "=" <+> d2

        lkp :: (PP a, PP b) => a -> b -> Doc
        lkp x y = pp i x <> brackets (pp i y)

        op1 :: (PP a) => String -> a -> Doc
        op1 o x = text o <> pp i x

        op2 :: (PP a, PP b) => String -> a -> b -> Doc
        op2 o x y = pp i x <+> text o <+> pp i y

        cond a x mb = text "if" <+> expr <+> text "then" <+> text "pc++"
                                         <+> mbElse
          where expr    = if a then text "not" <+> parens x else x
                mbElse  = maybe empty (\y -> text "else" <+> y) mb


    in
    case opCode of
      OP_MOVE a b -> a =: b

      OP_LOADK a b -> a =: b

      OP_LOADKX a -> a =: case ppExtraArg i of
                            Nothing -> text "extra"
                            Just x  -> pp i (Kst x)

      OP_LOADBOOL a b c -> (a =: b) <> more
         where more = if c then semi <+> text "pc++" else empty

      OP_LOADNIL a b  -> ppRegRange (ppNextPC i) a (b+1) =: text "nil"

      OP_GETUPVAL a b -> a =: b

      OP_GETTABUP a b c -> a =: lkp b c
      OP_GETTABLE a b c -> a =: lkp b c

      OP_SETTABUP a b c -> lkp a b =: c
      OP_SETUPVAL a b   -> b       =: a
      OP_SETTABLE a b c -> lkp a b =: c

      OP_NEWTABLE a b c -> a =: (text "{}" <+> parens (text "array size =" <+>
                                        int b <> comma <+>
                                        text "table size =" <+> int c))

      OP_SELF a b c -> (succ a =: b) <> semi <+> (a =: lkp b c)

      OP_ADD a b c  -> a =: op2 "+" b c
      OP_SUB a b c  -> a =: op2 "-" b c
      OP_MUL a b c  -> a =: op2 "*" b c
      OP_MOD a b c  -> a =: op2 "%" b c
      OP_POW a b c  -> a =: op2 "^" b c
      OP_DIV a b c  -> a =: op2 "/" b c
      OP_IDIV a b c -> a =: op2 "//" b c
      OP_BAND a b c -> a =: op2 "&" b c
      OP_BOR  a b c -> a =: op2 "|" b c
      OP_BXOR a b c -> a =: op2 "~" b c
      OP_SHL a b c  -> a =: op2 "<<" b c
      OP_SHR a b c  -> a =: op2 ">>" b c
      OP_UNM a b    -> a =: op1 "-" b
      OP_BNOT a b   -> a =: op1 "~" b
      OP_NOT a b    -> a =: (text "not" <+> pp i b)
      OP_LEN a b    -> a =: op1 "#" b

      OP_CONCAT a b@(Reg x) (Reg y) ->
         a =: text "concat" <+> ppRegRange i b (y - x + 1)

      OP_JMP mb b -> text "pc +=" <+> int b <+>
                     case mb of
                       Nothing -> empty
                       Just a  -> semi <+> text "close" <+> ppRegRangeInf i a

      OP_EQ a b c      -> cond a (op2 "==" b c) Nothing
      OP_LT a b c      -> cond a (op2 "<"  b c) Nothing
      OP_LE a b c      -> cond a (op2 "<=" b c) Nothing
      OP_TEST a c      -> cond c (pp i a)       Nothing
      OP_TESTSET a b c -> cond c (pp i b) (Just (a =: b))
      OP_CALL a b c -> ppRegRangeCount (ppNextPC i) a c =:
                            pp i a <> parens (ppRegRangeCount i (succ a) b)

      OP_TAILCALL a b _c ->
        text "return" <+> pp i a <> parens (ppRegRangeCount i (succ a) b)

      OP_RETURN a b -> text "return" <+> ppRegRangeCount i a b

      OP_FORPREP a b  -> text "FORPREP" <+> pp i a <+> int b
      OP_FORLOOP a b  -> text "FORLOOP" <+> pp i a <+> int b
      OP_TFORCALL a b -> text "FORCALL" <+> pp i a <+> int b
      OP_TFORLOOP a b -> text "FORLOOP" <+> pp i a <+> int b

      OP_SETLIST a b c ->
         pp (ppNextPC i) a <> brackets rng =: ppRegRange i (succ a) b
        where
        rng | c == 0    = conc (ppExtraArg i)
            | otherwise = conc (Just c)

        conc x | b == 1 = num x 1
        conc x          = num x 1 <> text ".." <> num x b
        num x j  = case x of
                     Just y  -> int ((y-1) * 50 + j)
                     Nothing -> text "50*(extra-1) +" <+> int j


      OP_CLOSURE a b -> a =: (text "closure" <+> pp i b)
      OP_VARARG a b  -> ppRegRangeCount (ppNextPC i) a b =: text "..."

      OP_EXTRAARG n ->
        case ppExtraArg i of
          Just _  -> empty
          Nothing -> nest 2 (text "where extra =" <+> int n)
