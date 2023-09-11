module Json
 ( Json(..),
   runJsonP,
 ) where

import Data.Char
import Parser

runJsonP :: Text -> Maybe Json
runJsonP = (fst <$>) . runParser (jsonArrP <|> jsonObjP)

data Json where
  JsonPrimative :: Text -> Json
  JsonArr :: [Json] -> Json
  JsonObj :: [(Text, Json)] -> Json

jsonP' :: Parser Json
jsonP' = asum [jsonNullP, jsonNumP, jsonStrP, jsonBoolP, jsonArrP, jsonObjP]

jsonNullP, jsonNumP, jsonStrP, jsonBoolP :: Parser Json
jsonNullP = strP "null"                                     <&> JsonPrimative
jsonNumP  = spanP (\c -> isDigit c || c == '.' || c == '-') <&> JsonPrimative
jsonStrP  = strLitP                                         <&> JsonPrimative
jsonBoolP = strP "true" <|> strP "false"                    <&> JsonPrimative

jsonArrP :: Parser Json
jsonArrP = charP '[' *> ws *> (JsonArr <$> elems) <* ws <* charP ']'
  where elems = commaSep jsonP'

jsonObjP :: Parser Json
jsonObjP = charP '{' *> ws *> (JsonObj <$> elems) <* ws <* charP '}'
  where elems = commaSep $ (,) <$> (strLitP <* sc) <*> jsonP'
        sc = ws <* charP ':' <* ws

commaSep :: Parser b -> Parser [b]
commaSep val = liftA2 (:) val (many (ws *> charP ',' *> ws *> val)) <|> pure []

strLitP :: Parser Text
strLitP = quote <$> (charP '"' *> spanP (/= '"') <* charP '"')
  where quote txt = fold ["\"", txt, "\""]

ws :: Parser Text
ws = spanP isSpace <|> pure mempty
