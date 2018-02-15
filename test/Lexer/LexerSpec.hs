{-# OPTIONS_GHC -Wall #-}

module Lexer.LexerSpec
  ( main
  , spec
  ) where

import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

import Lexer.Lexer
import Lexer.Token
import Lexer.TokenUtil (toStringToken)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "lexer" $ do
    describe "unit-tests" $ do
      it "should not fail on any input" $
        forAll
          (listOf1 arbitrary)
          (\string ->
             case alexScan (alexStartPos, '\n', [], string) 0 of
               AlexError _ -> False
               _ -> True)
      it "should match with the first 0" $ runAlex "000 abc" alexMonadScan `shouldBe` (Right $ IntegerToken 0)
      it "should identify integers" $ runAlex "100000" alexMonadScan `shouldBe` (Right $ IntegerToken 100000)
      it "should identify type identifiers" $ runAlex "Foo" alexMonadScan `shouldBe` (Right $ TypeIdentifier "Foo")
      it "should identify object identifiers" $ runAlex "bar" alexMonadScan `shouldBe` (Right $ ObjectIdentifier "bar")
      it "should identifiers with underscore" $
        runAlex "foo_bar" alexMonadScan `shouldBe` (Right $ ObjectIdentifier "foo_bar")
      describe "keywords" $ do
        it "should identify keywords" $ runAlex "let" alexMonadScan `shouldBe` Right LetKeyword
        it "should identify insensitive keywords" $ runAlex "cASe" alexMonadScan `shouldBe` Right CaseKeyword
        it "should treat True as a type" $ runAlex "TRue" alexMonadScan `shouldBe` (Right $ TypeIdentifier "TRue")
        it "should treat Let as a keyword" $ runAlex "Let" alexMonadScan `shouldBe` Right LetKeyword
      describe "operators" $ do
        it "should parse ( operator" $ runAlex "(" alexMonadScan `shouldBe` Right LeftParenthesesOperator
        it "should parse [ operator" $ runAlex "[" alexMonadScan `shouldBe` Right LeftBracesOperator
      describe "string" $ do
        it "should identify string identifiers" $
          runAlex "\"hello\"" alexMonadScan `shouldBe` (Right $ StringToken "hello")
        describe "unterminated strings" $ do
          it "should identify unterminated strings" $
            runAlex "\"this is \n not okay\"" alexMonadScan `shouldBe` Right UnterminatedStringErrorToken
          it "should identify untermianted strings" $
            runAlex "\"hello\nstring is unterminated\"\n" scanner `shouldBe`
            Right
              [ UnterminatedStringErrorToken
              , ObjectIdentifier "string"
              , ObjectIdentifier "is"
              , ObjectIdentifier "unterminated"
              , UnterminatedStringErrorToken
              ]
        it "can contain escaped newline character" $
          toStringToken "\"this is \\n okay\"" `shouldBe` StringToken "this is \n okay"
        it "can contain escaped newline character 2" $ toStringToken "\"a\\\nb\"" `shouldBe` StringToken "a\nb"
        it "can parse escape characters" $ toStringToken "\"\\b\\t\\n\\f\"" `shouldBe` StringToken "\b\t\n\f"
        it "can treat most escape characters as regular characters" $ toStringToken "\"\\c\"" `shouldBe` StringToken "c"
        it "cannot contain null characters" $ toStringToken "\" foo\\0ooo \"" `shouldBe` NullCharacterErrorToken
        it "can parse inner quotes" $ toStringToken "\"1\\\"2'3~4\\\"5\"" `shouldBe` StringToken "1\"2'3~4\"5"
        it "can parse inner quotes" $
          runAlex "\"1\\\"2'3~4\\\"5\"" alexMonadScan `shouldBe` (Right $ StringToken "1\"2'3~4\"5")
        it "cannot parse eof" $ runAlex "\"foo" alexMonadScan `shouldBe` Right EOFStringErrorToken
      describe "comments" $ do
        describe "-- " $ do
          it "should be ignored" $ runAlex "-- foo" testDidSkipped `shouldBe` Right True
          it "should ignore comments but parse the next value" $
            runAlex "-- foo \n foo" alexMonadScan `shouldBe` (Right $ ObjectIdentifier "foo")
        describe "(* *)" $ do
          it "should throw an error if it sees an unmatched token" $
            runAlex "*)" alexMonadScan `shouldBe` Right UnmatchedCommentToken
          it "should have an opening and a closing statement" $
            runAlex "(* foo *) hi" alexMonadScan `shouldBe` (Right $ ObjectIdentifier "hi")
          it "should have an opening and a closing statement" $
            runAlex "(* foo *) hi" alexMonadScan `shouldBe` (Right $ ObjectIdentifier "hi")
--          it "should be able to parse a multiline comment" $ -- todo make sure to deal with multiple parenthesis
--            runAlex
--              "(* models one-dimensional cellular automaton on a circle of finite radius\n   arrays are faked as Strings,\n   X's respresent live cells, dots represent dead cells,\n   no error checking is done *)\n"
--              testDidSkipped `shouldBe`
--            Right True
    describe "integration tests" $ do
      it "should have alexMonadScan parse only one token at a time" $
        runAlex "Foo Bar" alexMonadScan `shouldBe` (Right $ TypeIdentifier "Foo")
      it "should have scanner parse an item" $
        runAlex "Foo Bar" scanner `shouldBe` Right [TypeIdentifier "Foo", TypeIdentifier "Bar"]
      describe "class" $ do
        it "should parse a class (simple example)" $
          testScanner
            "class Main {\n\n};\n"
            [ClassKeyword, TypeIdentifier "Main", LeftCurlyBracesOperator, RightCurlyBracesOperator, SemicolonOperator]
        let createAssignment variableName typeName =
              [ObjectIdentifier variableName, ColonOperator, TypeIdentifier typeName]
        let xcarAssignment = createAssignment "xcar" "Int" ++ [SemicolonOperator]
        let xcdrAssignment = createAssignment "xcdr" "List" ++ [SemicolonOperator]
        let isNullDeclaration =
              [ ObjectIdentifier "isNil"
              , LeftParenthesesOperator
              , RightParenthesesOperator
              , ColonOperator
              , TypeIdentifier "Bool"
              , LeftCurlyBracesOperator
              , FalseKeyword
              , RightCurlyBracesOperator
              , SemicolonOperator
              ]
        let consType = TypeIdentifier "Cons"
        it "should parse a class (complicated example)" $
          testScanner
            "class Cons inherits List {\n    xcar : Int;\n    xcdr : List;\n    isNil() : Bool { false };\n};\n"
            ([ClassKeyword, consType, InheritsKeyword, TypeIdentifier "List", LeftCurlyBracesOperator] ++
             xcarAssignment ++ xcdrAssignment ++ isNullDeclaration ++ [RightCurlyBracesOperator, SemicolonOperator])
        it "should parse the initialization of an object" $
          testScanner
            "(new Cons).init(1,new Nil)"
            [ LeftParenthesesOperator
            , NewKeyword
            , consType
            , RightParenthesesOperator
            , PeriodOperator
            , ObjectIdentifier "init"
            , LeftParenthesesOperator
            , IntegerToken 1
            , CommaOperator
            , NewKeyword
            , TypeIdentifier "Nil"
            , RightParenthesesOperator
            ]
        it "should parse a method invoked in a class" $
          "f(foo: String, bar: String): String { foo + bar };\n" `runAlex` scanner `shouldBe`
          (Right $
           [ObjectIdentifier "f", LeftParenthesesOperator] ++
           createAssignment "foo" "String" ++
           [CommaOperator] ++
           createAssignment "bar" "String" ++
           [ RightParenthesesOperator
           , ColonOperator
           , TypeIdentifier "String"
           , LeftCurlyBracesOperator
           , ObjectIdentifier "foo"
           , PlusOperator
           , ObjectIdentifier "bar"
           , RightCurlyBracesOperator
           , SemicolonOperator
           ])
        it "should parse a method call from an object" $
          testScanner
            "e0.f(e1)"
            [ ObjectIdentifier "e0"
            , PeriodOperator
            , ObjectIdentifier "f"
            , LeftParenthesesOperator
            , ObjectIdentifier "e1"
            , RightParenthesesOperator
            ]
        it "should parse a method call from an object using it's parents" $
          testScanner
            "e0@ParentType.f(e1)"
            [ ObjectIdentifier "e0"
            , AtOperator
            , TypeIdentifier "ParentType"
            , PeriodOperator
            , ObjectIdentifier "f"
            , LeftParenthesesOperator
            , ObjectIdentifier "e1"
            , RightParenthesesOperator
            ]
        it "should parse an assignment" $
          testScanner
            "xcar: Int <- 1;"
            (createAssignment "xcar" "Int" ++ [AssignmentOperator, IntegerToken 1, SemicolonOperator])
        it "should parse an if else statement" $
          testScanner
            "if foo then bar else baz fi"
            [ IfKeyword
            , ObjectIdentifier "foo"
            , ThenKeyword
            , ObjectIdentifier "bar"
            , ElseKeyword
            , ObjectIdentifier "baz"
            , FiKeyword
            ]
        it "should parse a while loop" $
          testScanner
            "while x <= 10 loop x <- x - 1 pool"
            [ WhileKeyword
            , ObjectIdentifier "x"
            , LessThanOrEqualOperator
            , IntegerToken 10
            , LoopKeyword
            , ObjectIdentifier "x"
            , AssignmentOperator
            , ObjectIdentifier "x"
            , MinusOperator
            , IntegerToken 1
            , PoolKeyword
            ]
        it "should parse a let statement" $
          testScanner
            "let x : Integer <- 1; y: Integer in x + y"
            ([LetKeyword] ++
             createAssignment "x" "Integer" ++
             [AssignmentOperator, IntegerToken 1, SemicolonOperator] ++
             createAssignment "y" "Integer" ++ [InKeyword, ObjectIdentifier "x", PlusOperator, ObjectIdentifier "y"])
        let createTypeBound variableName typeName tokens =
              createAssignment variableName typeName ++ [TypeBoundOperator] ++ tokens ++ [SemicolonOperator]
        it "should parse a case statement" $
          testScanner
            "case self of\n\t\t  n : Razz => (new Bar);\n\t\t  n : Bar => n;\n\t\tesac;\n"
            ([CaseKeyword, ObjectIdentifier "self", OfKeyword] ++
             createTypeBound
               "n"
               "Razz"
               [LeftParenthesesOperator, NewKeyword, TypeIdentifier "Bar", RightParenthesesOperator] ++
             createTypeBound "n" "Bar" [ObjectIdentifier "n"] ++ [EsacKeyword, SemicolonOperator])
    describe "parse files" $ do
      it "can parse different types of white spaces" $
        testScanFile scanner "test/Lexer/Files/test1.cl" "test/Lexer/Files/test1.cl.out"
      it "can parse different typeIdentifiers, objectIdentifiers and integers" $
        testScanFile scanner "test/Lexer/Files/test2.cl" "test/Lexer/Files/test2.cl.out"
      it "can parse different strings" $
        testScanFile scanner "test/Lexer/Files/test3.cl" "test/Lexer/Files/test3.cl.out"
      it "can parse an entire program" $
        testScanFile scanner "test/Lexer/Files/test4.cl" "test/Lexer/Files/test4.cl.out"

testScanner :: String -> [Token] -> Expectation
testScanner code expectedTokens = code `runAlex` scanner `shouldBe` Right expectedTokens

testDidSkipped :: Alex Bool
testDidSkipped = do
  input <- alexGetInput
  startCode <- alexGetStartCode
  case alexScan input startCode of
    AlexSkip _ _ -> return True
    AlexEOF -> return True
    _ -> return False

testScanFile :: Alex [Token] -> String -> String -> Expectation
testScanFile lexer testFileName expectedResultsFileName = do
  expectedTokens <- readExpectedTestResults expectedResultsFileName
  fileContents <- readFile testFileName
  let testResults = runAlex fileContents lexer
  testResults `shouldBe` Right expectedTokens

readExpectedTestResults :: String -> IO [Token]
readExpectedTestResults outputFileName = do
  outputFile <- readFile outputFileName
  let outputStrings = lines outputFile
  return [read outputString | outputString <- outputStrings]
