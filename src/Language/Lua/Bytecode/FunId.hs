module Language.Lua.Bytecode.FunId where

import Data.List(intercalate)
import Text.PrettyPrint(text)
import Text.Read(readMaybe)


-- | Path from root to function.
newtype FunId = FunId [Int]
                deriving (Eq,Ord,Show)

isRootFun :: FunId -> Bool
isRootFun (FunId x) = case x of
                        [_] -> True
                        _   -> False

getRoot :: FunId -> Maybe Int
getRoot (FunId xs) = if null xs then Nothing else Just (last xs)

noFun :: FunId
noFun = FunId []

isNoFun :: FunId -> Bool
isNoFun (FunId xs) = null xs

rootFun :: Int -> FunId
rootFun n = FunId [n]

subFun :: FunId -> Int -> FunId
subFun (FunId xs) y = FunId (y : xs)

funIdString :: FunId -> String
funIdString (FunId xs) = intercalate "_" (map show (reverse xs))

funIdFromString :: String -> Maybe FunId
funIdFromString = fmap (FunId . reverse) . mapM readMaybe . words . map cvt
  where
  cvt x = if x == '_' then ' ' else x

funIdList :: FunId -> [Int]
funIdList (FunId xs) = reverse xs

funNestDepth :: FunId -> Int
funNestDepth (FunId xs) = length xs

