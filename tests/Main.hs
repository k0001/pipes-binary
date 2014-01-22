{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Test.Tasty            as Tasty
import           Test.Tasty.HUnit      ((@=?), testCase)
import qualified Test.Tasty.Runners    as Tasty
import           Test.Tasty.SmallCheck (forAll, testProperty)

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tests


tests :: Tasty.TestTree
tests = Tasty.testGroup "pipes-binary"
  [ testFunctorLaws
  ]


testFunctorLaws :: Tasty.TestTree
testFunctorLaws = Tasty.testGroup "Functor laws (sample test)"
  [ testCase "fmap id Nothing = Nothing" $ do
      fmap id Nothing @=? (Nothing :: Maybe ())
  , testProperty "fmap id = id" $ do
      forAll $ \(x :: [Int]) ->
        fmap id x == id x
  , testCase "fmap (f . g) Nothing = (fmap f . fmap g) Nothing" $ do
      fmap (not . not) Nothing @=? (fmap not . fmap not) (Nothing :: Maybe Bool)
  , testProperty "fmap (f . g) = fmap f . fmap g" $ do
      forAll $ \(x :: [Int]) ->
        fmap (odd . succ) x == (fmap odd . fmap succ) x
  ]

