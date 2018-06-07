{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=500 #-}
module BasicTests.CircuitDers where

import           ConCat.ADFun
import           ConCat.AltCat (toCcc, toCcc')
import qualified ConCat.AltCat as A
import           ConCat.Misc ((:*), (:+), R, C, Unop, Binop, magSqr, sqr, unzip)
import           ConCat.RAD
import           Miscellany hiding (runSynCirc, runSynCircDers)
import           Utils
import           Test.Tasty (TestTree, testGroup)

circuitDers :: TestTree
circuitDers =
  testGroup "automatic differentiation variants"
    [ runSynCircDers "add"     $ uncurry ((+) @R)
    , runSynCircDers "sin"     $ sin @R
    , runSynCircDers "cos"     $ cos @R
    , runSynCircDers "twice"   $ twice @R
    , runSynCircDers "sqr"     $ sqr @R
    , runSynCircDers "magSqr"  $ magSqr  @R
    , runSynCircDers "cos-2x"  $ \ x -> cos (2 * x) :: R
    , runSynCircDers "cos-2xx" $ \ x -> cos (2 * x * x) :: R
    , runSynCircDers "cos-xpy" $ \ (x,y) -> cos (x + y) :: R
    ]

runSynCircDers :: (GO a b, Num b) => String -> (a -> b) -> TestTree
runSynCircDers nm f =
  testGroup (nm ++ "-ders")
  [ {- runSynCirc nm               $ toCcc $ id       $ f
  , -} runSynCirc (nm ++ "-adf")   $ toCcc $ andDerF  $ f
  , runSynCirc (nm ++ "-adr")   $ toCcc $ andDerR  $ f
  , runSynCirc (nm ++ "-gradr") $ toCcc $ andGradR $ f
  ]
{-# INLINE runSynCircDers #-}
