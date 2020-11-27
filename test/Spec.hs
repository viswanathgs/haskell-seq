{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import System.Exit
import HMM

-- https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-All.html
return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- and <$> sequence [runTests]
  if success then exitSuccess else exitFailure

-- TODO
