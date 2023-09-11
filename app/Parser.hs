module Parser where

import Data.List
import Data.Text qualified as T

newtype Parser a = Parser
  { runParser :: Text -> Maybe (a, Text)
  } deriving (Functor)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ Just . (a,)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser f) <*> (Parser p) = Parser $ \str -> do
    (f', rest) <- f str
    (a, rest') <- p rest
    Just (f' a, rest')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser a) <|> (Parser b) = Parser $ liftA2 (<|>) a b

spanP :: (Char -> Bool) -> Parser Text
spanP predicate = Parser $ \input ->
  let result@(matched, _) = T.span predicate input
   in result <$ guard (not $ T.null matched)

charP :: Char -> Parser Text
charP c = Parser $ T.uncons >=> (\(x, xs) -> guard (x == c) $> (T.singleton c, xs))

strP :: String -> Parser Text
strP = foldl1' (liftA2 (<>)) . map charP
