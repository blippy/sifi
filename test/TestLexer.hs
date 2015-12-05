module TestLexer where

import Test.HUnit

import Lexer

lex01 = alexScanTokens "foo, bar baz \"fish stew for you\" \"today"

lex02 = alexScanTokens " # foo \"var "
test02 = TestCase (assertEqual "an empty line with comment" [] lex02)


tests = TestList
        [ TestLabel "test02" test02 ]

runTests = runTestTT tests        
