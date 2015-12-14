{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Parser where

import Language

import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>), parserZero, many, runParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import Text.Parsec.Char (letter, alphaNum, char)

lexer :: TokenParser ()
lexer = makeTokenParser langDef

langDef :: LanguageDef ()
langDef = emptyDef
  { commentStart = "(*"
  , commentEnd = "*)"
  , nestedComments = True
  , identStart = letter
  , identLetter = alphaNum <|> char '_'
  , opStart = parserZero
  , opLetter = parserZero
  , reservedNames = ["true", "false"]
  , reservedOpNames = ["+", "*", "/", "-", "%"]
  }

tokenParser :: Parser Term
tokenParser = (PushIntTerm . fromIntegral) <$> integer lexer
          <|> reserved lexer "true" *> pure (PushBoolTerm True)
          <|> reserved lexer "false" *> pure (PushBoolTerm False)
          <|> BuiltinTerm <$> identifier lexer
          <|> PushFuncTerm <$> braces lexer parser

parser :: Parser Term
parser = do whiteSpace lexer
            fs <- many $ lexeme lexer tokenParser
            if null fs
               then return IdTerm
               else return $ foldl1 CatTerm fs

parse :: String -> Either ParseError Term
parse = runParser parser () ""

