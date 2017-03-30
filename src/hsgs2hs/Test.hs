import Test.HUnit

import Control.Applicative (Alternative(..))

import System.Exit (ExitCode(..), exitWith)

import GSI.Util (Pos(..))

import HSGS.Parser (Parser, parse, pfail)

main = do
    let pos = Pos "./foo" 1 1
    c <- runTestTT $ TestList $ [
        let
            p :: Parser Char ()
            p = empty
        in TestCase $ assertEqual "parse/empty" (parse p pos "") (Left "./foo:1:1: Unexpected \"\""),
        let
            p :: Parser Char ()
            p = pfail "bleargh"
        in TestCase $ assertEqual "parse/empty" (parse p pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh")
      ]
    if errors c == 0 && failures c == 0 then exitWith ExitSuccess else exitWith (ExitFailure 1)
