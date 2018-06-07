{-# LANGUAGE TypeApplications #-}
module BasicTests.CircuitGraphs where

import           ConCat.AltCat (toCcc, toCcc')
import qualified ConCat.AltCat as A
import           ConCat.Misc ((:*), (:+), R, C, Unop, Binop, magSqr, sqr, unzip)
import           Miscellany hiding (runSynCirc, runSynCircDers)
import           Utils
import           Test.Tasty (TestTree, testGroup)

circuitGraphs :: TestTree
circuitGraphs =
  testGroup "circuit graphs"
    [ runSynCirc "add"         $ toCcc $ (+) @R
    , runSynCirc "add-uncurry" $ toCcc $ uncurry ((+) @R)
    , runSynCirc "dup"         $ toCcc $ A.dup @(->) @R
    , runSynCirc "fst"         $ toCcc $ fst @R @R
    , runSynCirc "twice"       $ toCcc $ twice @R
    , runSynCirc "sqr"         $ toCcc $ sqr @R
    , runSynCirc "complex-mul" $ toCcc $ uncurry ((*) @C)
    , runSynCirc "magSqr"      $ toCcc $ magSqr @R
    , runSynCirc "cosSinProd"  $ toCcc $ cosSinProd @R
    , runSynCirc "xp3y"        $ toCcc $ \ (x,y) -> x + 3 * y :: R
    , runSynCirc "horner"      $ toCcc $ horner @R [1,3,5]
    , runSynCirc "cos-2xx"     $ toCcc $ \ x -> cos (2 * x * x) :: R
    ]
