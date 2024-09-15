module Main where

import Control.Applicative
import Data.Char qualified as Char
import Parser
import System.Directory

main = do
  allFiles <- getDirectoryContents "./jsons"
  let files = drop 2 allFiles
  texts <- mapM (readFile . ("./jsons/" ++)) files
  let res = zip files (map valid texts)
  mapM_ print res

data Pair = Pair Key Value
  deriving (Show)

newtype Key = Key String
  deriving (Show)

data Value
  = S String
  | N String
  | L String
  | A [Value]
  | O [Pair]
  deriving (Show)

valid :: String -> Bool
valid xs = case runParser parseObj xs of
                Just(_, "") -> True
                otherwise -> False

parseObj :: Parser [Pair]
parseObj =
  (\_ y _ -> y)
    <$> (spaces *> char '{' <* spaces)
    <*> ( atleast0 (parsePair <* char ',' <* spaces)
            >>= ( \x ->
                    if null x
                      then (: []) <$> parsePair <|> pure []
                      else (\y -> x ++ [y]) <$> parsePair
                )
        )
    <*> (spaces *> char '}' <* spaces)

parsePair :: Parser Pair
parsePair =
  (\x _ y -> Pair x y)
    <$> (parseKey <* spaces)
    <*> char ':'
    <*> (spaces *> parseVal)

parseKey :: Parser Key
parseKey = Key <$> parseString

parseVal :: Parser Value
parseVal =
  S <$> parseString
    <|> N <$> parseNumber
    <|> L <$> parseLiteral
    <|> A <$> parseArray
    <|> O <$> parseObj

parseString :: Parser String
parseString = (\_ y _ -> y) <$> char '"' <*> atleast0 (satisfy (/= '"')) <*> char '"'

parseNumber :: Parser String
parseNumber = p5
  where
    p1 = (\x y z -> x ++ y ++ z) <$> safeChar '-' <*> digits <*> safeChar '.'
    p2 = p1 >>= (\x -> if last x == '.' then (x ++) <$> digits else pure x)
    p3 = (++) <$> p2 <*> safeChar 'e'
    p4 = p3 >>= (\x -> if last x == 'e' then (x ++) <$> safeChar '-' else pure x)
    p5 = p4 >>= (\x -> if last x == 'e' || last x == '-' then (x ++) <$> digits else pure x)


parseLiteral :: Parser String
parseLiteral =
  (join4 <$> char 't' <*> char 'r' <*> char 'u' <*> char 'e')
    <|> (join5 <$> char 'f' <*> char 'a' <*> char 'l' <*> char 's' <*> char 'e')
    <|> (join4 <$> char 'n' <*> char 'u' <*> char 'l' <*> char 'l')
  where
    join4 a b c d = [a] ++ [b] ++ [c] ++ [d]
    join5 a b c d e = [a] ++ [b] ++ [c] ++ [d] ++ [e]

parseArray :: Parser [Value]
parseArray =
  (\_ y _ -> y)
    <$> (char '[' <* spaces)
    <*> ( atleast0 (parseVal <* char ',' <* spaces)
            >>= ( \x ->
                    if null x
                      then (: []) <$> parseVal <|> pure []
                      else (\y -> x ++ [y]) <$> parseVal
                )
        )
    <*> (spaces *> char ']')
