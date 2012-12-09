module Main (main) where

import Test.HUnit hiding (Test)
import Test.QuickCheck hiding ((.&.))
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Data.LargeWord
import Data.Bits
import Control.Monad

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
   arbitrary = liftM2 LargeKey arbitrary arbitrary

pShiftRightShiftLeft :: Word128 -> Bool
pShiftRightShiftLeft x = shiftR (shiftL x 1) 1 == x .&. (fromInteger ((2^127) - 1))

u1 = shiftR (18446744073709551616  :: Word128) 64  @?= 1

tests :: [Test]
tests =
    [ testProperty "largeword shift left then right" pShiftRightShiftLeft
    , testCase "largeword shift 2^64 by 2^64" u1
      ]

main = defaultMain tests
