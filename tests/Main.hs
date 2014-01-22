{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Test.Tasty            as Tasty
import           Test.Tasty.HUnit      ((@=?), testCase)
import qualified Test.Tasty.Runners    as Tasty
import           Test.Tasty.SmallCheck (forAll, testProperty)

import Data.Int
import Data.Word
import qualified Data.Binary as Bin

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Lens.Family.State.Strict (zoom)
import Pipes ()
import qualified Pipes.Prelude as P
import qualified Pipes.Binary as PBin

--------------------------------------------------------------------------------

main :: IO ()
main =  Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tests


tests :: Tasty.TestTree
tests = Tasty.testGroup "root"
  [ testFunctorLaws
  , testPipesBinary
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


testPipesBinary :: Tasty.TestTree
testPipesBinary = Tasty.testGroup "pipes-binary"
  [ testProperty "Pipes.Binary.encode and Data.Binary.encode give same results" $ do
      forAll $ \(x :: (Char, (Double, (Int, (Maybe Int, Either Bool Int))))) ->
         BL.toStrict (Bin.encode x) == B.concat (P.toList (PBin.encode x))
  ]
