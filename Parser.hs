module Parser where

import Control.Applicative
import Data.Char qualified as Char

newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser f) = f

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | pred x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

safeChar :: Char -> Parser String
safeChar c = (: []) <$> satisfy (== c) <|> pure []

digits :: Parser String
digits = Parser f
  where
    f xs = case span Char.isDigit xs of
      ([], _) -> Nothing
      (nums, ys) -> Just (nums, ys)

instance Functor Parser where
  fmap g p = Parser f
    where
      f xs = case runParser p xs of
        Nothing -> Nothing
        Just (m, n) -> Just (g m, n)

instance Applicative Parser where
  pure x = Parser (\y -> Just (x, y))
  f <*> p = Parser r
    where
      r xs = case runParser f xs of
        Nothing -> Nothing
        Just (m, n) -> runParser (fmap m p) n

instance Monad Parser where
  p >>= k = Parser f
    where
      f x = case runParser p x of
        Nothing -> Nothing
        Just (m, n) -> runParser (k m) n

instance Alternative Parser where
  empty = Parser (const Nothing)
  p <|> q = Parser r
    where
      r xs = runParser p xs <|> runParser q xs

atleast0 :: Parser a -> Parser [a]
atleast0 p = atleast1 p <|> pure []

atleast1 :: Parser a -> Parser [a]
atleast1 p = (:) <$> p <*> atleast0 p

spaces :: Parser String
spaces = atleast0 (satisfy Char.isSpace)
