{-# LANGUAGE FlexibleInstances #-}

module Testing where

import qualified Data.Map as Map
import Data.Either (isRight)

import Test.HUnit hiding (Testable)
import Test.QuickCheck

import Debug.Trace

import Language
import Parser
import Builtin
import Inference
import Main

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

main :: IO ()
main =
  do _ <- runTestTT $ TestList [testParser, testTypes, testInterpret]
     quickCheckN 10000 prop_id
     quickCheckN 10000 prop_quote
     quickCheckN 10000 prop_associative
     quickCheckN 1000  prop_concat
     quickCheckN 10000 prop_wellTyped

testParser :: Test
testParser = TestList [testParseEmpty, testParseBuiltin, testParseQuotes]

testTypes :: Test
testTypes = TestList [testTypesBase, testTypesBuiltin,
                      testTypesQuotes, testTypesConcat]

testInterpret :: Test
testInterpret = TestList [testInterpretBase, testInterpretConcat]


-- Tests for parser

parsesTo :: String -> Term () -> Test
parsesTo s t =
  case parse s of
    Right t' -> t ~=? t'
    Left err -> False ~? show err

noParse :: String -> Test
noParse s =
  case parse s of
    Right _ -> False ~? "Should not parse: " ++ s
    Left _  -> True  ~? "Failed as expected"

testParseEmpty :: Test
testParseEmpty = "Parsing whitespace and no tokens" ~: TestList
  [ "Empty string"         ~: ""         `parsesTo` IdTerm ()
  , "Whitespace chars"     ~: " "        `parsesTo` IdTerm ()
  , "Lots of whitespace"   ~: "     "    `parsesTo` IdTerm ()
  , "Comment"              ~: "(* Hi *)" `parsesTo` IdTerm ()
  ]

testParseLiteral :: Test
testParseLiteral = "Parsing literal bool and int values" ~: TestList
  [ "Single digit numbers" ~: "1"        `parsesTo` PushIntTerm () 1
  , "Multi-digit numbers"  ~: "552"      `parsesTo` PushIntTerm () 552
  , "Negative numbers"     ~: "-12"      `parsesTo` PushIntTerm () (-12)
  , "True Boolean"         ~: "true"     `parsesTo` PushBoolTerm () True
  , "False Boolean"        ~: "false"    `parsesTo` PushBoolTerm () False
  , "Int with whitespace"  ~: " 10  "    `parsesTo` PushIntTerm () 10
  ]

testParseBuiltin :: Test
testParseBuiltin = "Parsing (presumed builtin) identifiers" ~: TestList
  [ "Single letter"        ~: "a"        `parsesTo` BuiltinTerm () "a"
  , "Single word"          ~: "foo"      `parsesTo` BuiltinTerm () "foo"
  , "Word with whitespace" ~: "   foo "  `parsesTo` BuiltinTerm () "foo"
  , "Word with underscore" ~: "ok_ay"    `parsesTo` BuiltinTerm () "ok_ay"
  ]

testParseQuotes :: Test
testParseQuotes = "Parsing quotations (first-class functions)" ~: TestList
  [ "Empty quotation"      ~: "{ }"      `parsesTo` PushFuncTerm () (IdTerm ())
  , "Empty quot no spaces" ~: "{}"       `parsesTo` PushFuncTerm () (IdTerm ())
  , "Quoted builtin"       ~: "{foo}"    `parsesTo`
        PushFuncTerm () (BuiltinTerm () "foo")
  , "Quoted and spaces"    ~: "{ foo }" `parsesTo`
        PushFuncTerm () (BuiltinTerm () "foo")
  , "Many quoted names"    ~: "{ f b }" `parsesTo`
        PushFuncTerm () (CatTerm () (BuiltinTerm () "f") (BuiltinTerm () "b"))
  , "Nested quotes"        ~: "{{ }}" `parsesTo`
        PushFuncTerm () (PushFuncTerm () $ IdTerm ())
  , "Mismatched brackets"  ~: noParse "{ foo"
  ]


-- Tests for type inference

-- Variable-independent comparison of two FuncTypes
infix 4 ~:~
(~:~) :: FuncType -> FuncType -> Bool
f ~:~ g = fty == gty
  where Right (fty, _) = runTC $ freshen f
        Right (gty, _) = runTC $ freshen g

hasType :: Term () -> FuncType -> Test
hasType term ty =
  case extract <$> typeInference term of
    Right ty' -> ty ~:~ ty' ~? show ty ++ "\n" ++ show ty'
    Left err  -> False      ~? show err

hasNoType :: Term () -> Test
hasNoType term =
  case typeInference term of
    Right _  -> False ~? "Should not typecheck: " ++ show term
    Left err -> True  ~? "Failed as expected"


int, bool, idType, plusType :: ValueType
int = VIntTy
bool = VBoolTy
idType = VFuncTy $ F (S "A" []) (S "A" [])
plusType = VFuncTy $ F (S "A" [int, int]) (S "A" [int])

pushes :: [ValueType] -> FuncType
pushes l = F (S "A" []) (S "A" $ go l)
  where go []               = []
        go (VFuncTy f : ts) = VFuncTy ty' : go ts
          where Right (ty', _) = runTC $ freshen f
        go (t : ts)         = t : go ts

testTypesBase :: Test
testTypesBase = "Type inference for base types" ~: TestList
  [ PushIntTerm () 0       `hasType` pushes [int]
  , PushIntTerm () 100     `hasType` pushes [int]
  , PushBoolTerm () True   `hasType` pushes [bool]
  , PushBoolTerm () False  `hasType` pushes [bool]
  ]

testTypesBuiltin :: Test
testTypesBuiltin = "Type inference for builtin functions" ~: TestList
  [ BuiltinTerm () "plus"  `hasType` F (S "A" [int, int]) (S "A" [int])
  , BuiltinTerm () "minus" `hasType` F (S "A" [int, int]) (S "A" [int])
  , BuiltinTerm () "times" `hasType` F (S "A" [int, int]) (S "A" [int])
  , hasNoType $ BuiltinTerm () "not_a_builtin"
  ]

testTypesQuotes :: Test
testTypesQuotes = "Type inference for quotations" ~: TestList
  [ PushFuncTerm () (IdTerm ())             `hasType` pushes [idType]
  , PushFuncTerm () (BuiltinTerm () "plus") `hasType` pushes [plusType]
  , PushFuncTerm () (PushIntTerm () 3)      `hasType`
        pushes [VFuncTy $ pushes [int]]
  , PushFuncTerm () (PushBoolTerm () False) `hasType`
        pushes [VFuncTy $ pushes [bool]]
  , PushFuncTerm () (PushFuncTerm () $ IdTerm ()) `hasType`
        pushes [VFuncTy $ pushes [idType]]
  , PushFuncTerm () (PushFuncTerm () (PushIntTerm () 3)) `hasType`
        pushes [VFuncTy $ pushes [VFuncTy $ pushes [int]]]
  ]

testTypesConcat :: Test
testTypesConcat = "Type inference for term concatentation" ~: TestList
  [ CatTerm () (IdTerm ()) (IdTerm ()) `hasType` pushes []
  , CatTerm () (IdTerm ()) (PushIntTerm () 3) `hasType` pushes [int]
  , CatTerm () (PushIntTerm () 3) (PushIntTerm () 2) `hasType` pushes [int, int]
  , CatTerm () (PushIntTerm () 3)
      (CatTerm () (PushBoolTerm () False) (PushBoolTerm () True))
        `hasType` pushes [int, bool, bool]
  , CatTerm () (CatTerm () (PushIntTerm () 3) (PushBoolTerm () False))
      (PushBoolTerm () True)
        `hasType` pushes [int, bool, bool]
  ]


-- Tests for interpreter

evalsTo :: Term () -> [Value] -> Test
term `evalsTo` expectedValues =
  case typeInference term of
    Right typedTerm ->
      let actualValues = reverse $ interpret typedTerm [] in
      (length expectedValues == length actualValues
        && valuesEqual actualValues expectedValues) ~?= True
    Left _ -> False ~? "Should be equal"

testInterpretBase :: Test
testInterpretBase = "Interpret base values" ~: TestList
  [ PushIntTerm () 0      `evalsTo` [IntVal 0]
  , PushIntTerm () 100    `evalsTo` [IntVal 100]
  , PushBoolTerm () False `evalsTo` [BoolVal False]
  , PushBoolTerm () True  `evalsTo` [BoolVal True]
  ]

push3, push5, pushf :: Term ()
push3 = PushIntTerm () 3
push5 = PushIntTerm () 5
pusht = PushBoolTerm () True
pushf = PushBoolTerm () False

testInterpretConcat :: Test
testInterpretConcat = "Interpret term concatentation" ~: TestList
  [ CatTerm () (IdTerm ()) (IdTerm ()) `evalsTo` []
  , CatTerm () (IdTerm ()) push5 `evalsTo` [IntVal 5]
  , CatTerm () push5 push3 `evalsTo` [IntVal 5, IntVal 3]
  , CatTerm () pushf push3 `evalsTo` [BoolVal False, IntVal 3]
  , CatTerm () (CatTerm () push5 push3) push5 `evalsTo`
      [IntVal 5, IntVal 3, IntVal 5]
  , CatTerm () (CatTerm () push3 pushf) (CatTerm () pusht push5) `evalsTo`
      [IntVal 3, BoolVal False, BoolVal True, IntVal 5]
  , CatTerm () (CatTerm () (CatTerm () push3 push5) push3) pusht `evalsTo`
      [IntVal 3, IntVal 5, IntVal 3, BoolVal True]
  , CatTerm () (CatTerm () push3 push5)
               (CatTerm () (PushFuncTerm () (BuiltinTerm () "plus"))
                           (BuiltinTerm () "apply2to1"))
      `evalsTo` [IntVal 8]
  ]


-- QuickCheck properties for type inference

instance Arbitrary (Term ()) where
  arbitrary = frequency
    [ (1, return $ IdTerm ())
    , (8, CatTerm () <$> arbitrary <*> arbitrary)
    , (4, BuiltinTerm () <$> elements (Map.keys builtins))
    , (4, PushIntTerm () <$> arbitrary)
    , (2, PushBoolTerm () <$> arbitrary)
    , (5, PushFuncTerm () <$> arbitrary)
    ]

  shrink (CatTerm () t1 t2)  = [t1, t2]
  shrink (PushFuncTerm () t) = [t]
  shrink _                   = []

prop_id :: Term () -> Bool
prop_id term =
  case extract <$> typeInference term of
    Right ty ->
      case (extract <$> typeInference cat1, extract <$> typeInference cat2) of
        (Right ty', Right ty'') -> ty ~:~ ty' && ty ~:~ ty''
        _ -> False
    Left _ -> discard
  where cat1 = CatTerm () (IdTerm ()) term
        cat2 = CatTerm () term (IdTerm ())

prop_quote :: Term () -> Bool
prop_quote term =
  case extract <$> typeInference term of
    Right ty ->
      case extract <$> typeInference (PushFuncTerm () term) of
        Right ty' -> ty' ~:~ pushes [VFuncTy ty]
        Left _    -> False
    Left _ -> discard

prop_associative :: Term () -> Term () -> Term () -> Bool
prop_associative term1 term2 term3 =
  case (extract <$> typeInference cat1, extract <$> typeInference cat2) of
    (Right ty1, Right ty2) -> ty1 ~:~ ty2
    (Left _, Left _) -> True
    _ -> False
  where cat1 = CatTerm () (CatTerm () term1 term2) term3
        cat2 = CatTerm () term1 (CatTerm () term2 term3)

prop_concat :: Term () -> Term () -> Bool
prop_concat term1 term2 =
  let ty1 = extract <$> typeInferenceOnEmpty term1
      ty2 = extract <$> typeInferenceOnEmpty term2
   in case (ty1, ty2) of
    (Right (F a b), Right (F c d)) ->
      if b == c then
        case extract <$> typeInferenceOnEmpty (CatTerm () term1 term2) of
          Right ty' -> ty' ~:~ F a d
          Left _    -> False
      else discard
    _ -> discard

prop_wellTyped :: Term () -> Bool
prop_wellTyped term =
  case typeInferenceOnEmpty term of
    Right typedTerm ->
      let F _ (S _ exp) = extract typedTerm
          resultStack = reverse $ interpret typedTerm []
          resStackTy = stackType resultStack
          resFunc = F (S "" []) resStackTy
      in F (S "" []) (S "" exp) ~:~ resFunc
    Left _ -> discard

stackType :: [Value] -> Stack
stackType = S "" . map valueType

valueType :: Value -> ValueType
valueType (IntVal _)    = VIntTy
valueType (BoolVal _)   = VBoolTy
valueType (ListVal t _) = VListTy t
valueType (FuncVal t _) = VFuncTy t

valueEqual :: Value -> Value -> Bool
valueEqual (IntVal i1)  (IntVal i2)      = i1 == i2
valueEqual (BoolVal b1) (BoolVal b2)     = b1 == b2
valueEqual (ListVal _ l1) (ListVal _ l2) = valuesEqual l1 l2
valueEqual (FuncVal _ _) (FuncVal _ _)   = error "Can't equate functions"

valuesEqual :: [Value] -> [Value] -> Bool
valuesEqual = (and .) . zipWith valueEqual

