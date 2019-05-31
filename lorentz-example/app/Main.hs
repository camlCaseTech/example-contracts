module Main where

import Data.Text.IO (hGetContents)
import GHC.IO.Encoding (textEncodingName)
import System.IO (TextEncoding, hGetEncoding, hSetBinaryMode, hSetEncoding, mkTextEncoding, utf8)

import Callback (printCallbackContract)
import Caller (printCallerContract)
import Lib (printLorentzExample)
import CallbackWithAView (printCallbackWithAViewContract)
import qualified CallAndCallback

-- ~/alphanet.sh client typecheck script container:token.tz

writeFileUtf8 :: Print text => FilePath -> text -> IO ()
writeFileUtf8 name txt =
  withFile name WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h txt


main :: IO ()
main = do
  writeFileUtf8 "example.tz" printLorentzExample
  writeFileUtf8 "caller.tz" printCallerContract
  writeFileUtf8 "callback.tz" printCallbackContract
  writeFileUtf8 "callbackWithAView.tz" printCallbackWithAViewContract
  writeFileUtf8 "callbackView.tz" CallAndCallback.printCallbackContract
  writeFileUtf8 "callerView.tz" CallAndCallback.printCallerContract
