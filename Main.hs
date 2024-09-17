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

  ff <- readFile "test.json"
  mapM_ print ff
  print (runParser parseVal ff)

-- mapM_ print res

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
valid xs = case runParser parseVal xs of
  Just (_, "") -> True
  _ -> False

parseObj :: Parser [Pair]
parseObj =
  ( (\_ _ -> [])
      <$> (spaces *> char '{' <* spaces)
      <*> (spaces *> char '}' <* spaces)
  )
    <|> ( (\_ x y _ -> x ++ [y])
            <$> (spaces *> char '{' <* spaces)
            <*> atleast0 (const <$> parsePair <*> char ',' <* spaces)
            <*> parsePair
            <*> (spaces *> char '}' <* spaces)
        )

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
  O <$> parseObj
    <|> A <$> parseArray
    <|> N <$> parseNumber
    <|> S <$> parseString
    <|> L <$> parseLiteral

parseString :: Parser String
parseString =
  (\_ y _ -> y)
    <$> (spaces *> char '"')
    <*> ( (atleast0 (satisfy isLegal))
            >>= ( \x ->
                    if last x == '\\'
                      --then (\y -> x ++ "hello") <$> (char 'r' <|> char 't')
                      then error "x"
                      else pure x
                )
        )
    <*> (char '"' <* spaces)
  where
    isLegal c = c == ' ' || not (c == '"' || c == '\\' || Char.isSpace c || Char.isControl c)

parseNumber :: Parser String
parseNumber = spaces *> p5 <* spaces
  where
    p1 = (\x y z -> x ++ y ++ z) <$> safeChar '-' <*> digits <*> safeChar '.'
    p2 = p1 >>= (\x -> if last x == '.' then (x ++) <$> digits else pure x)
    p3 = (++) <$> p2 <*> safeChar 'e'
    p4 = p3 >>= (\x -> if last x == 'e' then (x ++) <$> safeChar '-' else pure x)
    p5 = p4 >>= (\x -> if last x == 'e' || last x == '-' then (x ++) <$> digits else pure x)

parseLiteral :: Parser String
parseLiteral =
  (join4 <$> (spaces *> char 't') <*> char 'r' <*> char 'u' <*> (char 'e' <* spaces))
    <|> (join5 <$> (spaces *> char 'f') <*> char 'a' <*> char 'l' <*> char 's' <*> (char 'e' <* spaces))
    <|> (join4 <$> (spaces *> char 'n') <*> char 'u' <*> char 'l' <*> (char 'l' <* spaces))
  where
    join4 a b c d = [a] ++ [b] ++ [c] ++ [d]
    join5 a b c d e = [a] ++ [b] ++ [c] ++ [d] ++ [e]

parseArray :: Parser [Value]
parseArray =
  ( (\_ _ -> [])
      <$> (spaces *> char '[' <* spaces)
      <*> (spaces *> char ']' <* spaces)
  )
    <|> ( (\_ x y _ -> x ++ [y])
            <$> (spaces *> char '[' <* spaces)
            <*> atleast0 (const <$> parseVal <*> char ',' <* spaces)
            <*> parseVal
            <*> (spaces *> char ']' <* spaces)
        )
