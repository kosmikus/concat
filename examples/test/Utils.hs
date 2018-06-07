{-# OPTIONS_GHC -Wall #-}

module Utils where

import           ConCat.ADFun
import           ConCat.AltCat (toCcc)
import qualified ConCat.AltCat as A
import           ConCat.Circuit (mkGraph, graphDot)
import           ConCat.RAD
import           ConCat.Syntactic (render)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Semigroup ((<>))
import           Miscellany (GO, EC)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden


------------------------------------------------------------------------------
-- | Run gold tests for a CCC'd syntax and circuit graph dot.
runSynCirc :: GO a b => String -> EC a b -> TestTree
runSynCirc nm (syn A.:**: circ) =
  testGroup nm
    [ goldenVsString "syntax"
                     ("test/gold/" <> nm <> "-syn.golden")
        . pure
        . BS.pack
        $ render syn

    , goldenVsString ("circuit dot")
                     ("test/gold/" <> nm <> "-dot.golden")
        . pure
        . BS.pack
        $ graphDot nm [] (mkGraph circ)
    ]

