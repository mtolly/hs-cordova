module Main where

import Distribution.Simple
import Distribution.Simple.PreProcess
import System.Process (readProcess)

runRuby _ _ = PreProcessor
  { platformIndependent = True
  , runPreProcessor = mkSimplePreProcessor $ \erbFile fout verbosity -> do
    hsRedundant <- readProcess "erb" [erbFile] ""
    hsClean <- readProcess "rb/clean_imports.rb" [] hsRedundant
    writeFile fout hsClean
  }

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = [("erb", runRuby)]
  }
