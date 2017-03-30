import Test.HUnit

import Control.Applicative (Alternative(..))

import System.Exit (ExitCode(..), exitWith)

import GSI.Util (Pos(..))

import HSGS.Parser (Parser, parse, symbol, pfail)

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
        in TestCase $ assertEqual "parse/pfail" (parse p pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh"),
        let
            p :: Parser Char ()
            p = pfail "bleargh" <|> empty
        in TestCase $ assertEqual "parse/(pfail <|> empty)" (parse p pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh"),
        let
            p :: Parser Char ()
            p = empty <|> pfail "bleargh"
        in TestCase $ assertEqual "parse/(empty <|> pfail)" (parse p pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh"),
        let
            p :: Parser Char ()
            p = pfail "bleargh" <|> pfail "bleargh"
        in TestCase $ assertEqual "parse/(pfail <|> pfail)" (parse p pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh; bleargh"),
        TestCase $ assertEqual "parse/symbol/[]" (parse symbol pos "") (Left "./foo:1:1: Unexpected \"\"; expecting symbol"),
        TestCase $ assertEqual "parse/(pfail <|> symbol)/[]" (parse (pfail "bleargh" <|> symbol) pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh; expecting symbol"),
        TestCase $ assertEqual "parse/(symbol <|> pfail)/[]" (parse (symbol <|> pfail "bleargh") pos "") (Left "./foo:1:1: Unexpected \"\"; bleargh; expecting symbol")
      ]
    if errors c == 0 && failures c == 0 then exitWith ExitSuccess else exitWith (ExitFailure 1)
