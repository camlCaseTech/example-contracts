module Main where

import Callback (printCallbackContract)
import Caller (printCallerContract)
import Lib (printLorentzExample)

main :: IO ()
main = do
  writeFile "example.tz" printLorentzExample
  writeFile "caller.tz" printCallerContract
  writeFile "callback.tz" printCallbackContract
