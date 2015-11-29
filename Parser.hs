{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Parser where

import Expressions

import Data.Maybe (fromJust)

import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>), parserZero, many, runParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.Parsec.Char (letter, alphaNum, char)
import Text.Parsec.Error (ParseError)

primitives :: [(String, Function)]
primitives = stackPrimitives ++ listPrimitives

lexer :: TokenParser ()
lexer = makeTokenParser langDef

langDef :: LanguageDef ()
langDef = emptyDef
  { commentStart = "(*"
  , commentEnd = "*)"
  , commentLine = "//"
  , nestedComments = True
  , identStart = letter
  , identLetter = alphaNum <|> char '_'
  , opStart = parserZero
  , opLetter = parserZero
  , reservedNames = ["true", "false"]
  , reservedOpNames = ["+", "*", "/", "-", "%"]
  }

tokenParser :: Parser Function
tokenParser = (push . IntVal . fromIntegral) <$> integer lexer
          <|> (reserved lexer "true" *> pure (push $ BoolVal True))
          <|> (reserved lexer "false" *> pure (push $ BoolVal False))
          <|> flip (fromJust .: lookup) primitives <$> identifier lexer
          <|> (push . FunVal) <$> braces lexer parser

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

parser :: Parser Function
parser = do whiteSpace lexer
            fs <- many (lexeme lexer tokenParser)
            return $ Prelude.foldl (flip (.)) id fs


-- Quick and dirty testing function
run :: String -> Either ParseError [StackValue]
run s = runParser parser () "" s
        >>= \f -> return $ f []

