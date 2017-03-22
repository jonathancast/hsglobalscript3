{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (forM_)

import System.Environment (getArgs)

import GSI.Util (gsfatal)

main = do
    as <- getArgs
    forM_ as $ \ a -> do
        s <- readFile a
        case compileHSGSSource s of
            Left err -> $gsfatal $ "main " ++ show err ++ " next"
            Right s' -> do
                writeFile (mkHSFile a) s'

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
