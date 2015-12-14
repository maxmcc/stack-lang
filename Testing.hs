module Testing where

import qualified Data.Map as Map
import Data.Either (isRight)

import Test.QuickCheck
import Test.HUnit

import Language
import Parser
import Builtin
import Inference
import Main

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
    Left err -> False ~? show err

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

-- | Variable-independent comparison of two FuncTypes
infix 4 ~:~
(~:~) :: FuncType -> FuncType -> Bool
f ~:~ g = fty == gty
  where Right (fty, _) = runTC $ freshen f
        Right (gty, _) = runTC $ freshen g

hasType :: Term -> FuncType -> Test
hasType term ty =
  case typeInference term of
    Right ty' -> ty ~:~ ty' ~?= True
    Left _ -> False ~? "Should typecheck"

int, bool :: ValueType
int = VIntTy
bool = VBoolTy

testTypesBase :: Test
testTypesBase = "Type inference for base types" ~: TestList
  [ PushIntTerm 0       `hasType` F (S "A" []) (S "A" [int])
  , PushIntTerm 100     `hasType` F (S "A" []) (S "A" [int])
  , PushBoolTerm True   `hasType` F (S "A" []) (S "A" [bool])
  , PushBoolTerm False  `hasType` F (S "A" []) (S "A" [bool])
  ]

testTypesBuiltin :: Test
testTypesBuiltin = "Type inference for builtin functions" ~: TestList
  [ BuiltinTerm "plus"  `hasType` F (S "A" [int, int]) (S "A" [int])
  , BuiltinTerm "minus" `hasType` F (S "A" [int, int]) (S "A" [int])
  , BuiltinTerm "times" `hasType` F (S "A" [int, int]) (S "A" [int])
  ]

testTypesQuots :: Test
testTypesQuots = "Type inference for quotations" ~: TestList
  [
  ]



-- QC against reference interpreter

instance Arbitrary Term where
  arbitrary = frequency
    [ (2, return IdTerm)
    , (8, CatTerm <$> arbitrary <*> arbitrary)
    , (2, BuiltinTerm <$> elements (Map.keys builtins))
    , (3, PushIntTerm <$> arbitrary)
    , (2, PushBoolTerm <$> arbitrary)
    , (4, PushFuncTerm <$> arbitrary)
    ]

  shrink (CatTerm t1 t2)  = [t1, t2]
  shrink (PushFuncTerm t) = [t]
  shrink _                = []

-- Does the term have a type?
wellTyped :: Term -> Bool
wellTyped term = isRight $ typeInference term

prop_quote :: Term -> Bool
prop_quote term =
  case typeInference term of
    Right ty ->
      case typeInference (PushFuncTerm term) of
        Right ty' ->
          ty' ~:~ F (S "A" []) (S "A" [VFuncTy ty])
        Left err -> False
    Left _ -> discard

prop_wellTyped :: Term -> Bool
prop_wellTyped term =
  case typeInferenceOnEmpty term of
    Right ty -> slickify term [] `seq` True
    Left _ -> discard

