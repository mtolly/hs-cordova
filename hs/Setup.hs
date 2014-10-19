module Main where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Process (readProcess)

main = defaultMainWithHooks
  simpleUserHooks
    { hookedPreProcessors = [("erb", runErb)] }

runErb _ _ =
  PreProcessor
    { platformIndependent = True
    , runPreProcessor = mkSimplePreProcessor $ \fin fout verbosity -> do
      info verbosity $ "erb-processing " ++ fin ++ " to " ++ fout
      readProcess "erb" [fin] "" >>= writeFile fout
    }
