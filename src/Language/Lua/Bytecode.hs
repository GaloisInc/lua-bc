module Language.Lua.Bytecode where

import Data.Vector(Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

newtype Reg = Reg Int
 deriving (Read,Show,Eq,Ord)

instance Enum Reg where
  toEnum = Reg
  fromEnum (Reg r) = r

newtype UpIx = UpIx Int
 deriving (Read,Show,Eq,Ord)

newtype ProtoIx = ProtoIx Int
 deriving (Read,Show,Eq,Ord)

newtype Kst = Kst Int
 deriving (Read,Show,Eq,Ord)

data RK = RK_Reg Reg | RK_Kst Kst
 deriving (Read,Show,Eq,Ord)

data Chunk = Chunk Int Function -- ^ number of upvalues and function body
 deriving (Read,Show,Eq)

data OpCode
  = OP_MOVE     Reg Reg         {- ^    A B     R(A) := R(B)                                    -}
  | OP_LOADK    Reg Kst         {- ^    A Bx    R(A) := Kst(Bx)                                 -}
  | OP_LOADKX   Reg             {- ^    A       R(A) := Kst(extra arg)                          -}
  | OP_LOADBOOL Reg Bool Bool   {- ^    A B C   R(A) := (Bool)B; if (C) pc++                    -}
  | OP_LOADNIL  Reg Int         {- ^    A B     R(A), R(A+1), ..., R(A+B) := nil                -}
  | OP_GETUPVAL Reg UpIx        {- ^    A B     R(A) := UpValue[B]                              -}

  | OP_GETTABUP Reg UpIx RK     {- ^    A B C   R(A) := UpValue[B][RK(C)]                       -}
  | OP_GETTABLE Reg Reg RK      {- ^    A B C   R(A) := R(B)[RK(C)]                             -}

  | OP_SETTABUP UpIx RK RK      {- ^     A B C   UpValue[A][RK(B)] := RK(C)                      -}
  | OP_SETUPVAL Reg UpIx        {- ^     A B     UpValue[B] := R(A)                              -}
  | OP_SETTABLE Reg RK RK       {- ^    A B C   R(A)[RK(B)] := RK(C)                            -}

  | OP_NEWTABLE Reg Int Int     {- ^    A B C   R(A) := {} (size = B,C)                         -}

  | OP_SELF     Reg Reg RK      {- ^    A B C   R(A+1) := R(B); R(A) := R(B)[RK(C)]             -}

  | OP_ADD      Reg RK RK       {- ^    A B C   R(A) := RK(B) + RK(C)                           -}
  | OP_SUB      Reg RK RK       {- ^    A B C   R(A) := RK(B) - RK(C)                           -}
  | OP_MUL      Reg RK RK       {- ^    A B C   R(A) := RK(B) * RK(C)                           -}
  | OP_MOD      Reg RK RK       {- ^    A B C   R(A) := RK(B) % RK(C)                           -}
  | OP_POW      Reg RK RK       {- ^    A B C   R(A) := RK(B) ^ RK(C)                           -}
  | OP_DIV      Reg RK RK       {- ^    A B C   R(A) := RK(B) / RK(C)                           -}
  | OP_IDIV     Reg RK RK       {- ^    A B C   R(A) := RK(B) // RK(C)                          -}
  | OP_BAND     Reg RK RK       {- ^    A B C   R(A) := RK(B) & RK(C)                           -}
  | OP_BOR      Reg RK RK       {- ^    A B C   R(A) := RK(B) | RK(C)                           -}
  | OP_BXOR     Reg RK RK       {- ^    A B C   R(A) := RK(B) ~ RK(C)                           -}
  | OP_SHL      Reg RK RK       {- ^    A B C   R(A) := RK(B) << RK(C)                          -}
  | OP_SHR      Reg RK RK       {- ^    A B C   R(A) := RK(B) >> RK(C)                          -}
  | OP_UNM      Reg Reg         {- ^    A B     R(A) := -R(B)                                   -}
  | OP_BNOT     Reg Reg         {- ^    A B     R(A) := ~R(B)                                   -}
  | OP_NOT      Reg Reg         {- ^    A B     R(A) := not R(B)                                -}
  | OP_LEN      Reg Reg         {- ^    A B     R(A) := length of R(B)                          -}

  | OP_CONCAT   Reg Reg Reg     {- ^    A B C   R(A) := R(B).. ... ..R(C)                       -}

  | OP_JMP      (Maybe Reg) Int {- ^    A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)  -}
  | OP_EQ       Bool RK RK      {- ^    A B C   if ((RK(B) == RK(C)) ~= A) then pc++            -}
  | OP_LT       Bool RK RK      {- ^    A B C   if ((RK(B) <  RK(C)) ~= A) then pc++            -}
  | OP_LE       Bool RK RK      {- ^    A B C   if ((RK(B) <= RK(C)) ~= A) then pc++            -}

  | OP_TEST     Reg Bool        {- ^    A C     if not (R(A) <=> C) then pc++                   -}
  | OP_TESTSET  Reg Reg Bool    {- ^    A B C   if (R(B) <=> C) then R(A) := R(B) else pc++     -}

  | OP_CALL     Reg Count Count {- ^    A B C   R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1)) -}
  | OP_TAILCALL Reg Count Count {- ^    A B C   return R(A)(R(A+1), ... ,R(A+B-1))              -}
  | OP_RETURN   Reg Count       {- ^    A B     return R(A), ... ,R(A+B-2)      (see note)      -}

  | OP_FORLOOP  Reg Int         {- ^    A sBx   R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }-}
  | OP_FORPREP  Reg Int         {- ^    A sBx   R(A)-=R(A+2); pc+=sBx                           -}

  | OP_TFORCALL Reg Int         {- ^    A C     R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));  -}
  | OP_TFORLOOP Reg Int         {- ^    A sBx   if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }-}

  | OP_SETLIST  Reg Int Int   {- ^    A B C   R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B        -}

  | OP_CLOSURE  Reg ProtoIx     {- ^    A Bx    R(A) := closure(KPROTO[Bx])                     -}

  | OP_VARARG   Reg Count {- ^    A B     R(A), R(A+1), ..., R(A+B-2) = vararg            -}

  | OP_EXTRAARG Int             {- ^    Ax      extra (larger) argument for previous opcode     -}
  deriving (Read, Show, Eq, Ord)

data Count = CountInt Int | CountTop
  deriving (Read, Show, Eq, Ord)

data Function = Function
  { funcSource          :: !(Maybe ByteString)
  , funcLineDefined     :: !Int
  , funcLastLineDefined :: !Int
  , funcNumParams       :: !Int
  , funcIsVararg        :: !Bool
  , funcMaxStackSize    :: !Int
  , funcCode            :: !(Vector OpCode)
  , funcConstants       :: !(Vector Constant)
  , funcUpvalues        :: !(Vector Upvalue)
  , funcProtos          :: !(Vector Function)
  , funcDebug           :: !DebugInfo
  }
  deriving (Read, Show, Eq)

data Constant = KNil | KBool Bool | KNum Double | KInt Int | KString ByteString | KLongString ByteString
  deriving (Read, Show, Eq, Ord)

data Upvalue = UpReg Reg | UpUp UpIx
  deriving (Read, Show, Eq, Ord)

type LineNumber = Int

data DebugInfo = DebugInfo
  { debugInfoLines      :: !(Vector LineNumber)
  , debugInfoVars       :: !(Vector VarInfo)
  , debugInfoUpvalues   :: !(Vector ByteString)
  }
  deriving (Read, Show, Eq)

data VarInfo = VarInfo
  { varInfoName :: !ByteString
  , varInfoStart, varInfoEnd :: !Int
  }
  deriving (Read, Show, Eq)

propagateSources :: Function -> Function
propagateSources = go B.empty
  where
  go name func =
    case funcSource func of
      Nothing ->
         func { funcSource = (Just name)
              , funcProtos = fmap (go name) (funcProtos func) }
      Just src ->
         func { funcProtos = fmap (go src) (funcProtos func) }


-- | Compute a register relative to another.
plusReg :: Reg -> Int {- ^ offset -} -> Reg
plusReg (Reg i) j = Reg (i+j)

-- | Compute the distance between two registers.
diffReg :: Reg -> Reg -> Int
diffReg (Reg i) (Reg j) = i - j

-- | Compute a list of registers given a startin register and length.
regRange :: Reg {- ^ start -} -> Int {- ^ length -} -> [Reg]
regRange start n = take n [start ..]

