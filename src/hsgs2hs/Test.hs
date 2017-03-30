import Test.HUnit

import Control.Applicative (Alternative(..))

import System.Exit (ExitCode(..), exitWith)

import GSI.Util (Pos(..))

import HSGS.Parser (parse)

main = do
    c <- runTestTT $ TestList $ [
        TestCase $ assertEqual "parse/empty" (parse (empty *> return ()) (Pos "./foo" 1 1) "") (Left "./foo:1:1: parse error")
      ]
    if errors c == 0 && failures c == 0 then exitWith ExitSuccess else exitWith (ExitFailure 1)
