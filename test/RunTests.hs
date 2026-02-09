module Main (main) where

import Test.HUnit

import ConstantsTest (constantsTests)
import LogicTest (logicTests)
import MathTest (mathTests)
import ParserTest (parserTests)

allTests :: Test
allTests = TestList [parserTests, constantsTests, logicTests, mathTests]

main :: IO ()
main = runTestTTAndExit allTests
