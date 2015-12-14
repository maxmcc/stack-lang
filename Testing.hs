module Testing where

import qualified Data.Map as Map

import Test.QuickCheck
import Test.HUnit

import Terms
import Parser
import Builtin

testParser :: Test
testParser = TestList [testParseEmpty, testParseBuiltin, testParseQuotes]

testTypes :: Test
testTypes = TestList []

testInterpret :: Test
testInterpret = TestList []


-- tests for parser

parsesTo :: String -> Term -> Test
parsesTo s t =
  case parse s of
    Right t' -> t ~=? t'
    Left _ -> False ~? "Should parse"

noParse :: String -> Test
noParse s =
  case parse s of
    Right _ -> False ~? "Should not parse"
    Left _ -> True ~? "Failed as expected"

testParseEmpty :: Test
testParseEmpty = "Parsing whitespace and no tokens" ~: TestList
  [ "Empty string"         ~: ""         `parsesTo` IdTerm
  , "Whitespace chars"     ~: " "        `parsesTo` IdTerm
  , "Lots of whitespace"   ~: "     "    `parsesTo` IdTerm
  , "Comment"              ~: "(* Hi *)" `parsesTo` IdTerm
  ]

testParseLiteral :: Test
testParseLiteral = "Parsing literal bool and int values" ~: TestList
  [ "Single digit numbers" ~: "1"        `parsesTo` PushIntTerm 1
  , "Multi-digit numbers"  ~: "552"      `parsesTo` PushIntTerm 552
  , "Negative numbers"     ~: "-12"      `parsesTo` PushIntTerm (-12)
  , "True Boolean"         ~: "true"     `parsesTo` PushBoolTerm True
  , "False Boolean"        ~: "false"    `parsesTo` PushBoolTerm False
  , "Int with whitespace"  ~: " 10  "    `parsesTo` PushIntTerm 10
  ]

testParseBuiltin :: Test
testParseBuiltin = "Parsing (presumed builtin) identifiers" ~: TestList
  [ "Single letter"        ~: "a"        `parsesTo` BuiltinTerm "a"
  , "Single word"          ~: "foo"      `parsesTo` BuiltinTerm "foo"
  , "Word with whitespace" ~: "   foo "  `parsesTo` BuiltinTerm "foo"
  , "Word with underscore" ~: "ok_ay"    `parsesTo` BuiltinTerm "ok_ay"
  ]

testParseQuotes :: Test
testParseQuotes = "Parsing quotations (first-class functions)" ~: TestList
  [ "Empty quotation"      ~: "{ }"      `parsesTo` PushFuncTerm IdTerm
  , "Empty quot no spaces" ~: "{}"       `parsesTo` PushFuncTerm IdTerm
  , "Quoted builtin"       ~: "{foo}"    `parsesTo`
        PushFuncTerm (BuiltinTerm "foo")
  , "Quoted and spaces"    ~: "{ foo }" `parsesTo`
        PushFuncTerm (BuiltinTerm "foo")
  , "Many quoted names"    ~: "{ foo bar }" `parsesTo`
        PushFuncTerm (CatTerm (BuiltinTerm "foo") (BuiltinTerm "bar"))
  , "Nested quotes"        ~: "{{ }}" `parsesTo`
        PushFuncTerm (PushFuncTerm IdTerm)
  , "Mismatched brackets"  ~: noParse "{ foo"
  ]


-- tests for typechecker


-- tests for interpreter


-- QC against reference interpreter

builtins :: Map.Map String Int
builtins = Map.fromList [("plus", 0), ("times", 0), ("minus", 0)]

instance Arbitrary Term where
  arbitrary = frequency
    [ (2, return IdTerm)
    , (8, CatTerm <$> arbitrary <*> arbitrary)
    , (2, BuiltinTerm <$> elements (Map.keys builtins))
    , (3, PushIntTerm <$> arbitrary)
    , (2, PushBoolTerm <$> arbitrary)
    , (4, PushFuncTerm <$> arbitrary)
    ]

  shrink (CatTerm x y) = [x, y]
  shrink t = [t]


prop_wellTyped :: Term -> Property
prop_wellTyped term = undefined

