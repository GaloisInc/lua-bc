{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Lua.Bytecode.Parser
  ( -- * Parsers
    parseLuaBytecode
  , parseLuaBytecodeFile

  -- * Dump
  , dumpLuaBytecode
  , dumpLuaBytecodeFile

  -- * Settings
  , BytecodeMode(..)
  , Sizet(..)
  , LuacVersion(..)
  , luaBytecodeMode53
  ) where

import           Control.Applicative
import           Control.Monad (when, guard)
import           Control.Exception
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Binary.IEEE754 (putFloat64le, getFloat64le)
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.Foldable (traverse_)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Word (Word8, Word32)
import           Numeric (showHex)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.IO
import           System.IO.Error
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import           Language.Lua.Bytecode

type Puts a = a -> Put

data LuacVersion = Luac52 | Luac53
  deriving (Eq, Ord, Show, Read)

data Sizet = Sizet32 | Sizet64

sizetSize :: Sizet -> Int
sizetSize Sizet32 = 4
sizetSize Sizet64 = 8

data BytecodeMode = BytecodeMode
  { bytecodeVersion :: !LuacVersion
  , bytecodeSizet   :: !Sizet
  }

parseLuaBytecode :: Maybe String {- ^ Optional source name -} ->
                    L.ByteString {- ^ Bytecodes -} ->
                    Either String Chunk
parseLuaBytecode mbName bc =
  case runGetOrFail loadChunk bc of
    Left  (_,_,e)     -> Left e
    Right (_,_,chunk) -> Right (maybe chunk (setName chunk) mbName)
  where
  setName (Chunk n f) nm = Chunk n (setSourceName nm f)

dumpLuaBytecode :: BytecodeMode -> Chunk -> L.ByteString
dumpLuaBytecode mode = runPut . saveChunk mode

------------------------------------------------------------------------

parseLuaBytecodeFile :: FilePath -> IO (Either String Chunk)
parseLuaBytecodeFile fp = parseLuaBytecode (Just fp) <$> L.readFile fp

dumpLuaBytecodeFile :: FilePath -> Chunk -> IO ()
dumpLuaBytecodeFile fp luafile =
  L.writeFile fp (dumpLuaBytecode luaBytecodeMode53 luafile)

luaBytecodeMode53 :: BytecodeMode
luaBytecodeMode53 = BytecodeMode Luac53 Sizet64

------------------------------------------------------------------------

setSourceName :: String -> Function -> Function
setSourceName name func = func { funcSource = Just (packUtf8 name) }

------------------------------------------------------------------------

unpackUtf8 :: ByteString -> String
unpackUtf8 = Text.unpack . decodeUtf8

packUtf8 :: String -> ByteString
packUtf8 = encodeUtf8 . Text.pack

-- magic numbers -------------------------------------------------------

luaSignature :: ByteString
luaSignature = "\x1bLua"

mkLuaVersion :: Word8 -> Word8 -> Word8
mkLuaVersion major minor
  | major < 16, minor < 16 = major * 16 + minor
  | otherwise = error "mkLuaVersion: Bad arguments"

luacFormat :: Word8
luacFormat = 0

luacData :: ByteString
luacData = "\x19\x93\r\n\x1a\n"

luacInt :: Int
luacInt = 0x5678

luacNum :: Double
luacNum = 370.5

intSize :: Word8
intSize = 4

instructionSize :: Word8
instructionSize = intSize

luaIntegerSize :: Word8
luaIntegerSize = 8

luaNumberSize :: Word8
luaNumberSize = 8

-- top-level lua chunk file codec --------------------------------------

loadChunk :: Get Chunk
loadChunk =
  do mode <- loadHeader
     case bytecodeVersion mode of
       Luac52 -> do f <- loadFunction mode
                    return (Chunk (Vector.length (funcUpvalues f)) f)
       Luac53 -> do n <- fromIntegral <$> loadByte
                    f <- loadFunction mode
                    return (Chunk n f)

saveChunk :: BytecodeMode -> Puts Chunk
saveChunk mode (Chunk n f) =
  do saveHeader mode
     saveByte (fromIntegral n)
     saveFunction mode f

-- size_t codec --------------------------------------------------------

loadSizeT :: BytecodeMode -> Get Int
loadSizeT mode =
  case bytecodeSizet mode of
    Sizet64 -> fromIntegral <$> getWord64host
    Sizet32 -> fromIntegral <$> getWord32host

saveSizeT :: BytecodeMode -> Puts Int
saveSizeT mode =
  case bytecodeSizet mode of
    Sizet64 -> putWord64host . fromIntegral
    Sizet32 -> putWord32host . fromIntegral

-- byte codec ----------------------------------------------------------

loadString :: BytecodeMode -> Get (Maybe ByteString)
loadString mode =
  case bytecodeVersion mode of
    Luac52 ->
      do n <- loadSizeT mode
         if n == 0
           then pure Nothing -- NULL?
           else Just . B.init <$> getByteString n

    Luac53 ->
      do n <- loadByte
         mb <- case n of
                 0    -> return Nothing -- NULL?
                 0xff -> Just <$> loadSizeT mode
                 _    -> return (Just (fromIntegral n))
         traverse (\n' -> getByteString (n'-1)) mb

loadString' :: String -> BytecodeMode -> Get ByteString
loadString' name mode =
  do mb <- loadString mode
     case mb of
       Nothing -> fail name
       Just x  -> return x

saveString :: BytecodeMode -> Puts (Maybe ByteString)
saveString _ Nothing = saveByte 0
saveString mode (Just x) =
  do let n = B.length x
     if n < 0xfe
        then saveByte (fromIntegral n+1)
        else saveByte 0xff >> saveSizeT mode (n+1)
     putByteString x

saveString' :: BytecodeMode -> Puts ByteString
saveString' mode = saveString mode . Just

-- byte codec ----------------------------------------------------------

loadByte :: Get Word8
loadByte = getWord8

saveByte :: Puts Word8
saveByte = putWord8

-- lua integer value codec ---------------------------------------------

loadInteger :: Get Int
loadInteger = fromIntegral <$> getWord64host

saveInteger :: Puts Int
saveInteger = putWord64host . fromIntegral

-- lua int codec -------------------------------------------------------

loadInt :: Get Int
loadInt = fromIntegral <$> getWord32host

saveInt :: Puts Int
saveInt = putWord32host . fromIntegral

-- lua number codec ----------------------------------------------------

loadNumber :: Get Double
loadNumber = getFloat64le -- I'd use the "host" version if there was one

saveNumber :: Puts Double
saveNumber = putFloat64le -- I'd use the "host" version if there was one

-- boolean codec -------------------------------------------------------

loadBool :: Get Bool
loadBool = fmap (/= 0) loadByte

saveBool :: Puts Bool
saveBool x = saveByte (if x then 1 else 0)

-- array codec ---------------------------------------------------------

loadVectorOf :: Get a -> Get (Vector a)
loadVectorOf p =
  do n  <- loadInt
     Vector.replicateM n p

saveVectorOf :: Puts a -> Puts (Vector a)
saveVectorOf put xs =
  do saveInt (Vector.length xs)
     traverse_ put xs

-- chunk header codec --------------------------------------------------


(<?>) :: Get a -> String -> Get a
m <?> str = m <|> fail str

string :: ByteString -> Get ()
string xs =
  do ys <- getByteString (B.length xs)
     guard (xs == ys)

word8 :: Word8 -> Get ()
word8 x =
  do y <- getWord8
     guard (x == y)

loadHeader :: Get BytecodeMode
loadHeader =
  do string luaSignature <?> "signature"
     version <- getWord8
     if        version == mkLuaVersion 5 3 then loadHeader53
       else if version == mkLuaVersion 5 2 then loadHeader52
       else                                     fail "Unknown version"

loadHeader52 :: Get BytecodeMode
loadHeader52 =
  do word8 luacFormat         <?> "luac format"
     word8 1                  <?> "endianess"
     word8 intSize            <?> "int size"
     sizet <- getSizetSize
     word8 instructionSize    <?> "Instruction size"
     word8 luaNumberSize      <?> "Number size"
     word8 0                  <?> "Integral Number"
     string luacData          <?> "corruption check"
     return BytecodeMode
        { bytecodeVersion = Luac52
        , bytecodeSizet   = sizet
        }

loadHeader53 :: Get BytecodeMode
loadHeader53 =
  do word8 luacFormat         <?> "luac format"
     string luacData          <?> "corruption check"
     word8 intSize            <?> "int size"
     sizet <- getSizetSize
     word8 instructionSize    <?> "Instruction size"
     word8 luaIntegerSize     <?> "Integer size"
     word8 luaNumberSize      <?> "Number size"
     luaInteger luacInt       <?> "Integer encoding"
     luaNumber luacNum        <?> "Number encoding"
     return BytecodeMode
         { bytecodeVersion = Luac53
         , bytecodeSizet   = sizet
         }
  where

  luaInteger x =
    do y <- loadInteger
       guard (x == y)

  luaNumber x =
    do y <- loadNumber
       guard (x == y)

getSizetSize :: Get Sizet
getSizetSize =
  do n <- getWord8
     case n of
        8 -> return Sizet64
        4 -> return Sizet32
        _ -> fail ("Bad size_t size: " ++ show n)

saveHeader :: BytecodeMode -> Put
saveHeader mode =
  case bytecodeVersion mode of
    Luac52 -> error "saveHeader not implemented for version 5.2"
    Luac53 ->
      do putByteString luaSignature
         saveByte (mkLuaVersion 5 3)
         saveByte luacFormat
         putByteString luacData
         saveByte intSize
         saveByte (fromIntegral (sizetSize (bytecodeSizet mode)))
         saveByte instructionSize
         saveByte luaIntegerSize
         saveByte luaNumberSize
         saveInteger luacInt
         saveNumber luacNum

-- function codec ------------------------------------------------------

loadFunction :: BytecodeMode -> Get Function
loadFunction mode =
  case bytecodeVersion mode of
    Luac52 ->
      do funcLineDefined     <- loadInt
         funcLastLineDefined <- loadInt
         funcNumParams       <- fromIntegral <$> loadByte
         funcIsVararg        <- loadBool
         funcMaxStackSize    <- fromIntegral <$> loadByte
         funcCode            <- loadVectorOf (loadInstruction mode)
         funcConstants       <- loadVectorOf (loadConstant mode)
         funcProtos          <- loadVectorOf (loadFunction mode)
         funcUpvalues        <- loadVectorOf loadUpvalue
         funcSource          <- loadString mode
         funcDebug           <- loadDebugInfo mode
         return Function{..}

    Luac53 -> Function
          <$> loadString mode
          <*> loadInt
          <*> loadInt
          <*> (fromIntegral <$> loadByte)
          <*> loadBool
          <*> (fromIntegral <$> loadByte)
          <*> loadVectorOf (loadInstruction mode)
          <*> loadVectorOf (loadConstant mode)
          <*> loadVectorOf loadUpvalue
          <*> loadVectorOf (loadFunction mode)
          <*> loadDebugInfo mode

saveFunction :: BytecodeMode -> Puts Function
saveFunction mode Function{..} =
  do saveString mode funcSource
     saveInt funcLineDefined
     saveInt funcLastLineDefined
     saveByte (fromIntegral funcNumParams)
     saveBool funcIsVararg
     saveByte (fromIntegral funcMaxStackSize)
     saveVectorOf saveInstruction funcCode
     saveVectorOf (saveConstant mode) funcConstants
     saveVectorOf saveUpvalue funcUpvalues
     saveVectorOf (saveFunction mode) funcProtos
     saveDebugInfo mode funcDebug

-- function debug information codec ------------------------------------

loadDebugInfo :: BytecodeMode -> Get DebugInfo
loadDebugInfo mode = DebugInfo
  <$> loadVectorOf loadInt
  <*> loadVectorOf (loadVarInfo mode)
  <*> loadVectorOf (fromMaybe "UNKNOWN" <$> loadString mode)

saveDebugInfo :: BytecodeMode -> Puts DebugInfo
saveDebugInfo mode DebugInfo{..} =
  do saveVectorOf saveInt debugInfoLines
     saveVectorOf (saveVarInfo mode) debugInfoVars
     saveVectorOf (saveString' mode) debugInfoUpvalues

-- function constant codec ---------------------------------------------

loadConstant :: BytecodeMode -> Get Constant
loadConstant mode =
  do tag <- loadByte
     case tag of
       0  -> return KNil
       1  -> KBool       <$> loadBool
       3  -> KNum        <$> loadNumber
       4  -> KString     <$> loadString' "short constant" mode -- short
       19 -> KInt        <$> loadInteger
       20 -> KLongString <$> loadString' "long constant" mode -- long
       _  -> fail ("unknown constant type: " ++ show tag)

saveConstant :: BytecodeMode -> Puts Constant
saveConstant mode x =
  case x of
    KNil          -> saveByte 0
    KBool b       -> saveByte 1  >> saveBool b
    KNum n        -> saveByte 3  >> saveNumber n
    KString s     -> saveByte 4  >> saveString' mode s
    KInt i        -> saveByte 19 >> saveInteger i
    KLongString s -> saveByte 20 >> saveString' mode s

-- function upvalue codec ----------------------------------------------

loadUpvalue :: Get Upvalue
loadUpvalue =
  do instack <- loadBool
     idx     <- fromIntegral <$> loadByte
     return (if instack then UpReg (Reg idx) else UpUp (UpIx idx))

saveUpvalue :: Puts Upvalue
saveUpvalue x =
  case x of
    UpReg (Reg  idx) -> saveBool True  >> saveByte (fromIntegral idx)
    UpUp  (UpIx idx) -> saveBool False >> saveByte (fromIntegral idx)

-- debug var info codec ------------------------------------------------

loadVarInfo :: BytecodeMode -> Get VarInfo
loadVarInfo mode = VarInfo
  <$> loadString' "local variable" mode
  <*> loadInt
  <*> loadInt

saveVarInfo :: BytecodeMode -> Puts VarInfo
saveVarInfo mode VarInfo{..} =
  do saveString' mode varInfoName
     saveInt varInfoStart
     saveInt varInfoEnd

-- instruction codec ---------------------------------------------------

loadInstruction :: BytecodeMode -> Get OpCode
loadInstruction mode =
  case bytecodeVersion mode of
    Luac52 -> loadInstruction52
    Luac53 -> loadInstruction53

loadInstruction52 :: Get OpCode
loadInstruction52 =
  do raw <- getWord32host
     let op = extractPart size_OP pos_OP raw
         a  = extractPart size_A  pos_A  raw
         ax = extractPart size_Ax pos_Ax raw
         b  = extractPart size_B  pos_B  raw
         bx = extractPart size_Bx pos_Bx raw
         c  = extractPart size_B  pos_C  raw
         rk x | testBit x (size_B-1) = RK_Kst (Kst (clearBit x (size_B-1)))
              | otherwise            = RK_Reg (Reg x)
         sBx = bx - ((1 `shiftL` (size_Bx-1)) - 1)
     case op of
       0  -> return $! OP_MOVE (Reg a) (Reg b)
       1  -> return $! OP_LOADK (Reg a) (Kst bx)
       2  -> return $! OP_LOADKX (Reg a)
       3  -> return $! OP_LOADBOOL (Reg a) (b /= 0) (c /= 0)
       4  -> return $! OP_LOADNIL  (Reg a) b
       5  -> return $! OP_GETUPVAL (Reg a) (UpIx b)

       6  -> return $! OP_GETTABUP (Reg a) (UpIx b) (rk c)
       7  -> return $! OP_GETTABLE (Reg a) (Reg b) (rk c)

       8  -> return $! OP_SETTABUP (UpIx a) (rk b) (rk c)
       9  -> return $! OP_SETUPVAL (Reg a) (UpIx b)
       10 -> return $! OP_SETTABLE (Reg a) (rk b) (rk c)

       11 -> return $! OP_NEWTABLE (Reg a) (fb2int b) (fb2int c)

       12 -> return $! OP_SELF     (Reg a) (Reg b) (rk c)

       13 -> return $! OP_ADD      (Reg a) (rk b) (rk c)
       14 -> return $! OP_SUB      (Reg a) (rk b) (rk c)
       15 -> return $! OP_MUL      (Reg a) (rk b) (rk c)
       16 -> return $! OP_DIV      (Reg a) (rk b) (rk c)
       17 -> return $! OP_MOD      (Reg a) (rk b) (rk c)
       18 -> return $! OP_POW      (Reg a) (rk b) (rk c)
       19 -> return $! OP_UNM      (Reg a) (Reg b)
       20 -> return $! OP_NOT      (Reg a) (Reg b)
       21 -> return $! OP_LEN      (Reg a) (Reg b)

       22 -> return $! OP_CONCAT   (Reg a) (Reg b) (Reg c)

       23 -> let r | a == 0 = Nothing
                   | otherwise = Just (Reg (a-1))
             in return $! OP_JMP r sBx
       24 -> return $! OP_EQ        (a /= 0) (rk b) (rk c)
       25 -> return $! OP_LT        (a /= 0) (rk b) (rk c)
       26 -> return $! OP_LE        (a /= 0) (rk b) (rk c)

       27 -> return $! OP_TEST      (Reg a) (c /= 0)
       28 -> return $! OP_TESTSET   (Reg a) (Reg b) (c /= 0)

       29 -> return $! OP_CALL      (Reg a) (mkCount b) (mkCount c)
       30 -> return $! OP_TAILCALL  (Reg a) (mkCount b) (mkCount c)
       31 -> return $! OP_RETURN    (Reg a) (mkCount b)

       32 -> return $! OP_FORLOOP   (Reg a) sBx
       33 -> return $! OP_FORPREP   (Reg a) sBx

       34 -> return $! OP_TFORCALL  (Reg a) c
       35 -> return $! OP_TFORLOOP  (Reg a) sBx

       36 -> return $! OP_SETLIST  (Reg a) b c

       37 -> return $! OP_CLOSURE  (Reg a) (ProtoIx bx)

       38 -> return $! OP_VARARG   (Reg a) (mkCount b)

       39 -> return $! OP_EXTRAARG ax

       _  -> fail   $ "Bad instruction: 0x" ++ showHex raw ""

loadInstruction53 :: Get OpCode
loadInstruction53 =
  do raw <- getWord32host
     let op = extractPart size_OP pos_OP raw
         a  = extractPart size_A  pos_A  raw
         ax = extractPart size_Ax pos_Ax raw
         b  = extractPart size_B  pos_B  raw
         bx = extractPart size_Bx pos_Bx raw
         c  = extractPart size_B  pos_C  raw
         rk x | testBit x (size_B-1) = RK_Kst (Kst (clearBit x (size_B-1)))
              | otherwise            = RK_Reg (Reg x)
         sBx = bx - ((1 `shiftL` (size_Bx-1)) - 1)
     case op of
       0  -> return $! OP_MOVE (Reg a) (Reg b)
       1  -> return $! OP_LOADK (Reg a) (Kst bx)
       2  -> return $! OP_LOADKX (Reg a)
       3  -> return $! OP_LOADBOOL (Reg a) (b /= 0) (c /= 0)
       4  -> return $! OP_LOADNIL  (Reg a) b
       5  -> return $! OP_GETUPVAL (Reg a) (UpIx b)

       6  -> return $! OP_GETTABUP (Reg a) (UpIx b) (rk c)
       7  -> return $! OP_GETTABLE (Reg a) (Reg b) (rk c)

       8  -> return $! OP_SETTABUP (UpIx a) (rk b) (rk c)
       9  -> return $! OP_SETUPVAL (Reg a) (UpIx b)
       10 -> return $! OP_SETTABLE (Reg a) (rk b) (rk c)

       11 -> return $! OP_NEWTABLE (Reg a) (fb2int b) (fb2int c)

       12 -> return $! OP_SELF     (Reg a) (Reg b) (rk c)

       13 -> return $! OP_ADD      (Reg a) (rk b) (rk c)
       14 -> return $! OP_SUB      (Reg a) (rk b) (rk c)
       15 -> return $! OP_MUL      (Reg a) (rk b) (rk c)
       16 -> return $! OP_MOD      (Reg a) (rk b) (rk c)
       17 -> return $! OP_POW      (Reg a) (rk b) (rk c)
       18 -> return $! OP_DIV      (Reg a) (rk b) (rk c)
       19 -> return $! OP_IDIV     (Reg a) (rk b) (rk c)
       20 -> return $! OP_BAND     (Reg a) (rk b) (rk c)
       21 -> return $! OP_BOR      (Reg a) (rk b) (rk c)
       22 -> return $! OP_BXOR     (Reg a) (rk b) (rk c)
       23 -> return $! OP_SHL      (Reg a) (rk b) (rk c)
       24 -> return $! OP_SHR      (Reg a) (rk b) (rk c)
       25 -> return $! OP_UNM      (Reg a) (Reg b)
       26 -> return $! OP_BNOT     (Reg a) (Reg b)
       27 -> return $! OP_NOT      (Reg a) (Reg b)
       28 -> return $! OP_LEN      (Reg a) (Reg b)

       29 -> return $! OP_CONCAT   (Reg a) (Reg b) (Reg c)

       30 -> let r | a == 0 = Nothing
                   | otherwise = Just (Reg (a-1))
             in return $! OP_JMP r sBx
       31 -> return $! OP_EQ        (a /= 0) (rk b) (rk c)
       32 -> return $! OP_LT        (a /= 0) (rk b) (rk c)
       33 -> return $! OP_LE        (a /= 0) (rk b) (rk c)

       34 -> return $! OP_TEST      (Reg a) (c /= 0)
       35 -> return $! OP_TESTSET   (Reg a) (Reg b) (c /= 0)

       36 -> return $! OP_CALL      (Reg a) (mkCount b) (mkCount c)
       37 -> return $! OP_TAILCALL  (Reg a) (mkCount b) (mkCount c)
       38 -> return $! OP_RETURN    (Reg a) (mkCount b)

       39 -> return $! OP_FORLOOP   (Reg a) sBx
       40 -> return $! OP_FORPREP   (Reg a) sBx

       41 -> return $! OP_TFORCALL  (Reg a) c
       42 -> return $! OP_TFORLOOP  (Reg a) sBx

       43 -> return $! OP_SETLIST  (Reg a) b c

       44 -> return $! OP_CLOSURE  (Reg a) (ProtoIx bx)

       45 -> return $! OP_VARARG   (Reg a) (mkCount b)

       46 -> return $! OP_EXTRAARG ax

       _  -> fail   $ "Bad instruction: 0x" ++ showHex raw ""

mkCount :: Int -> Count
mkCount 0 = CountTop
mkCount x = CountInt (x-1)

saveInstruction :: Puts OpCode
saveInstruction x = putWord32host $ sum $
  case x of
    OP_MOVE     a b        -> [asOp  0, asA a, asB b]
    OP_LOADK    a bx       -> [asOp  1, asA a, asBX bx]
    OP_LOADKX   a          -> [asOp  2, asA a]
    OP_LOADBOOL a b c      -> [asOp  3, asA a, asB b, asC c]
    OP_LOADNIL  a b        -> [asOp  4, asA a, asB b]
    OP_GETUPVAL a b        -> [asOp  5, asA a, asB b]

    OP_GETTABUP a b c      -> [asOp  6, asA a, asB b, asC c]
    OP_GETTABLE a b c      -> [asOp  7, asA a, asB b, asC c]

    OP_SETTABUP a b c      -> [asOp  8, asA a, asB b, asC c]
    OP_SETUPVAL a b        -> [asOp  9, asA a, asB b]
    OP_SETTABLE a b c      -> [asOp 10, asA a, asB b, asC c]

    OP_NEWTABLE a b c      -> [asOp 11, asA a, asB (int2fb b), asC (int2fb c)]

    OP_SELF     a b c      -> [asOp 12, asA a, asB b, asC c]

    OP_ADD      a b c      -> [asOp 13, asA a, asB b, asC c]
    OP_SUB      a b c      -> [asOp 14, asA a, asB b, asC c]
    OP_MUL      a b c      -> [asOp 15, asA a, asB b, asC c]
    OP_MOD      a b c      -> [asOp 16, asA a, asB b, asC c]
    OP_POW      a b c      -> [asOp 17, asA a, asB b, asC c]
    OP_DIV      a b c      -> [asOp 18, asA a, asB b, asC c]
    OP_IDIV     a b c      -> [asOp 19, asA a, asB b, asC c]
    OP_BAND     a b c      -> [asOp 20, asA a, asB b, asC c]
    OP_BOR      a b c      -> [asOp 21, asA a, asB b, asC c]
    OP_BXOR     a b c      -> [asOp 22, asA a, asB b, asC c]
    OP_SHL      a b c      -> [asOp 23, asA a, asB b, asC c]
    OP_SHR      a b c      -> [asOp 24, asA a, asB b, asC c]
    OP_UNM      a b        -> [asOp 25, asA a, asB b]
    OP_BNOT     a b        -> [asOp 26, asA a, asB b]
    OP_NOT      a b        -> [asOp 27, asA a, asB b]
    OP_LEN      a b        -> [asOp 28, asA a, asB b]

    OP_CONCAT   a b c      -> [asOp 29, asA a, asB b, asC c]

    OP_JMP      Nothing sbx -> [asOp 30, asSBX sbx]
    OP_JMP      (Just (Reg a)) sbx -> [asOp 30, asA (a+1), asSBX sbx]

    OP_EQ       a b c      -> [asOp 31, asA a, asB b, asC c]
    OP_LT       a b c      -> [asOp 32, asA a, asB b, asC c]
    OP_LE       a b c      -> [asOp 33, asA a, asB b, asC c]

    OP_TEST     a c        -> [asOp 34, asA a, asC c]
    OP_TESTSET  a b c      -> [asOp 35, asA a, asB b, asC c]

    OP_CALL     a b c      -> [asOp 36, asA a, asB b, asC c]
    OP_TAILCALL a b c      -> [asOp 37, asA a, asB b, asC c]
    OP_RETURN   a b        -> [asOp 38, asA a, asB b]

    OP_FORLOOP  a sbx      -> [asOp 39, asA a, asSBX sbx]
    OP_FORPREP  a sbx      -> [asOp 40, asA a, asSBX sbx]

    OP_TFORCALL a c        -> [asOp 41, asA a, asC c]
    OP_TFORLOOP a sbx      -> [asOp 42, asA a, asSBX sbx]

    OP_SETLIST  a b c      -> [asOp 43, asA a, asB b, asC c]

    OP_CLOSURE  a bx       -> [asOp 44, asA a, asBX bx]

    OP_VARARG   a b        -> [asOp 45, asA a, asB b]

    OP_EXTRAARG ax         -> [asOp 46, asAX ax]

class    OpArg a       where opArg             :: a -> Word32
instance OpArg Reg     where opArg (Reg x)     = fromIntegral x
instance OpArg UpIx    where opArg (UpIx x)    = fromIntegral x
instance OpArg Kst     where opArg (Kst x)     = fromIntegral x
instance OpArg ProtoIx where opArg (ProtoIx x) = fromIntegral x
instance OpArg Int     where opArg             = fromIntegral
instance OpArg Word32  where opArg             = id
instance OpArg Bool    where opArg True        = 1
                             opArg False       = 0
instance OpArg RK      where opArg (RK_Reg r)  = opArg r
                             opArg (RK_Kst k)  = setBit (opArg k) (size_B-1)
instance OpArg Count   where opArg CountTop    = 0
                             opArg (CountInt x) = fromIntegral x + 1

asOp :: Word32 -> Word32
asOp = createPart size_OP pos_OP

asA :: OpArg a => a -> Word32
asA = createPart size_A pos_A . opArg

asB :: OpArg a => a -> Word32
asB = createPart size_B pos_B . opArg

asC :: OpArg a => a -> Word32
asC = createPart size_C pos_C . opArg

asSBX :: Int -> Word32
asSBX = createPart size_Bx pos_Bx . fromIntegral . correct
  where
  correct x = x + ((1 `shiftL` (size_Bx-1)) - 1)

asBX :: OpArg a => a -> Word32
asBX = createPart size_Bx pos_Bx . opArg

asAX :: OpArg a => a -> Word32
asAX = createPart size_Ax pos_Ax . opArg

extractPart :: Int -> Int -> Word32 -> Int
extractPart size pos x = fromIntegral ((x `shiftL` (32-size-pos)) `shiftR` (32-size))

createPart :: Int -> Int -> Word32 -> Word32
createPart size pos x = (mask .&. x) `shiftL` pos
  where
  mask = 1 `shiftL` size - 1

{- |
 converts an integer to a "floating point byte", represented as
 (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
 eeeee != 0 and (xxx) otherwise.
-}
fb2int :: Int -> Int
fb2int x
  | x < 8 = x
  | otherwise = (8 + (x .&. 7)) * 2^(shiftR x 3 - 1)

int2fb :: Int -> Word32
int2fb x0
  | x0 < 0 = error "int2fb: negative argument"
  | x0 < 8 = fromIntegral x0
  | otherwise = coarse 0 (fromIntegral x0)
  where
  coarse e x
    | x >= 8 `shiftL` 4 = coarse (e+4) ((x + 0xf) `shiftR` 4)
    | otherwise = fine e x
  fine e x
    | x >= 8 `shiftL` 1 = fine (e+1) ((x + 1) `shiftR` 1)
    | otherwise = finish e x
  finish e x = (e+1) `shiftL` 3 .|. (x - 8)

size_A, size_Ax, size_B, size_Bx, size_C, size_OP :: Int
pos_A, pos_Ax, pos_B, pos_Bx, pos_C, pos_OP :: Int
pos_A            = (pos_OP + size_OP)
pos_Ax           = pos_A
pos_B            = (pos_C + size_C)
pos_Bx           = pos_C
pos_C            = (pos_A + size_A)
pos_OP           = 0
size_A           = 8
size_Ax          = (size_C + size_B + size_A)
size_B           = 9
size_C           = 9
size_Bx          = (size_C + size_B)
size_OP          = 6
