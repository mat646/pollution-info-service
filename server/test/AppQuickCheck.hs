module AppQuickCheck where

import Test.QuickCheck
import App

main :: IO ()
main = quickCheck prop_unwrap

prop_unwrap :: Maybe String -> Bool
prop_unwrap xs = unwrapTime xs == unwrapQuickCheck xs

unwrapQuickCheck :: Maybe String -> String
unwrapQuickCheck time =
  case time of
    Nothing -> "null"
    Just x -> x
