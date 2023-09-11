module Main where

import Control.Exception
import Formatter
import Json
import System.IO (hPutStrLn)

main :: IO ()
main = getArgs >>= \case
  [ftype, path] | ftype `elem` ["minify", "pretty"] ->
    processJson ftype (toText path)

  _ -> crash "usage: 'jsonh <\"minify\"|\"pretty\"> <json|file path>'"

processJson :: String -> Text -> IO ()
processJson ftype toParse =
  let fileReadTry = tryReadFile $ toString toParse
   in fileReadTry >>= printFormatted ftype . runJsonP . fromMaybe toParse

printFormatted :: String -> Maybe Json -> IO ()
printFormatted ftype = maybe
  (crash "error parsing json (or invalid file path)")
  (putTextLn . getFormatType ftype)

getFormatType :: String -> Json -> Text
getFormatType "minify" = minify
getFormatType "pretty" = pretty

tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile filePath =
  let result = try @SomeException $ decodeUtf8 <$> readFileBS filePath
   in result <&> rightToMaybe

crash :: String -> IO b
crash = hPutStrLn stderr >=> const exitFailure
