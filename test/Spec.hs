module Main where

import Test.Hspec

import qualified Parser.CustomSpec
import qualified Parser.ISharesSpec
import qualified Parser.StateStreetSpec

main :: IO ()
main = hspec $ do
  Parser.CustomSpec.spec
  Parser.ISharesSpec.spec
  Parser.StateStreetSpec.spec
