module Formatter
  ( minify,
    pretty,
  ) where

import Data.Text qualified as T
import Json

minify :: Json -> Text
minify (JsonPrimative txt) = txt
minify (JsonArr arr) = fold ["[", intercalateMap "," minify arr, "]"]
minify (JsonObj obj) = fold ["{", intercalateMap "," kvpair obj, "}"]
  where kvpair (k, v) = fold [k, ":", minify v]

pretty :: Json -> Text
pretty = pretty' 1

pretty' :: Int -> Json -> Text
pretty' _ (JsonPrimative txt) = txt
pretty' ind (JsonArr arr) = enclose "[]" ind $ intercalateMap ",\n" ((spaces ind <>) . pretty' (ind + 1)) arr
pretty' ind (JsonObj obj) = enclose "{}" ind $ intercalateMap ",\n" kvpair obj
  where kvpair (k, v) = fold [spaces ind, k, ": ", pretty' (ind + 1) v]

spaces :: Int -> Text
spaces n = T.replicate (n * 2) " "

intercalateMap :: Text -> (a -> Text) -> [a] -> Text
intercalateMap sep fn elems = T.intercalate sep $ map fn elems

enclose :: String -> Int -> Text -> Text
enclose [o, c] ind elems = fold [T.singleton o, "\n", elems, "\n", spaces (ind - 1), T.singleton c]
