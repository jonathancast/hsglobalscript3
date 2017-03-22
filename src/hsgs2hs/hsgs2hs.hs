{-# LANGUAGE TemplateHaskell #-}

import Data.List (isSuffixOf)

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)

import GSI.Util (gsfatal)

main = do
    as <- getArgs
    mapM processArg as

processArg a = do
    id <- doesDirectoryExist a
    irf <- doesFileExist a
    if id then do
        as <- getDirectoryContents a
        mapM_ processArg $ map (\ a' -> a ++ '/' : a') $ filter (\ a' -> a' /= "." && a' /= "..") as
      else if irf && ".hsgs" `isSuffixOf` a then do
        s <- readFile a
        case compileHSGSSource s of
            Left err -> $gsfatal $ "main " ++ show err ++ " next"
            Right s' -> do
                writeFile (mkHSFile a) s'
      else do
        return ()

compileHSGSSource :: String -> Either String String
compileHSGSSource s =
   splitInput s >>=
   compileSource >>=
   formatOutput >>=
   return . concat

mkHSFile :: FilePath -> FilePath
mkHSFile ".hsgs" = ".hs"
mkHSFile (c:s) = c:mkHSFile s

formatOutput :: [DestComp] -> Either String [String]
formatOutput [] = return []
formatOutput dcs = $gsfatal $ "formatOutput " ++ show dcs ++ " next"

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource [] = return []
compileSource scs = $gsfatal $ "compileSource " ++ show scs ++ " next"

splitInput :: String -> Either String [SourceComp]
splitInput "" = return []
splitInput s = $gsfatal $ "splitInput " ++ show s ++ " next"

data SourceComp = SourceComp
   deriving Show

data DestComp = DestComp
   deriving Show
