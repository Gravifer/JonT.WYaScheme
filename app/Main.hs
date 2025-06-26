{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
-- import GHC.Base
import Control.Monad
import Text.ParserCombinators.Parsec -- // hiding (spaces)
import System.Environment
-- import System.Console.ANSI
import Prettyprinter
import Prettyprinter.Render.Terminal
import Numeric (readHex)
import Data.Char (chr)
import Data.Functor((<&>))
import Lib

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- // spaces :: Parser ()
-- // spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

parseString :: Parser LispVal
parseString = between (char '"') (char '"') (many stringChar <&> String)

-- Match either a normal character or an escape sequence
stringChar :: Parser Char
stringChar = try parseEscape <|>noneOf "\"\\"

-- Parses escape sequences like \" \\ \n \t \r \u263A
-- * [R5RS Section 6.3.5] Scheme does not specify the effect of a backslash within a string that is not followed by a doublequote or backslash.
-- Spec: https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html
parseEscape :: Parser Char
parseEscape = do
  _ <- char '\\'
  esc <- oneOf "\\\"tnrvfbau\'"
  case esc of
    '\\' -> return '\\'
    '"'  -> return '"'
    -- extensions; see https://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
    -- https://en.wikipedia.org/wiki/String_literal#Escape_sequences:~:text=x22%5Cn%22-,Escape%20Sequence,Literal%20Characters%20placed%20into%20string,-%5C0
    'a'  -> return '\a'
    'b'  -> return '\b'
    'f'  -> return '\f'
    'n'  -> return '\n'
    'r'  -> return '\r'
    't'  -> return '\t'
    'v'  -> return '\v'
    '\'' -> return '\''
    -- 'u'  -> parseUnicode
    x@_  -> return x -- unexpected $ "invalid escape sequence: \\" ++ [esc]

-- Parses Unicode escape like \u263A
parseUnicode :: Parser Char
parseUnicode = do
  hexDigits <- count 4 hexDigit
  let [(code, _)] = readHex hexDigits
  return $ chr code

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

main :: IO ()
main = do
  (expr:_) <- getArgs
  putDoc $ annotate (colorDull Cyan) "parsing: [\n    "
  putStr expr
  putDoc $ annotate (colorDull Cyan) "\n  ]" <> line
  putStrLn $ readExpr expr

