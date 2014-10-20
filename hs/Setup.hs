module Main where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Process (readProcess)
import System.Environment (setEnv)
import System.FilePath (splitPath, isPathSeparator, dropExtension, (</>))

main = defaultMainWithHooks
  simpleUserHooks
    { hookedPreProcessors = [("erb", \_ _ -> PreProcessor
      { platformIndependent = True
      , runPreProcessor = runErb
      })]
    }

runErb (din, fin) (dout, fout) verbosity = do
  let fin' = din </> fin
      fout' = dout </> fout
      modName
        = filter (not . isPathSeparator)
        $ intercalate "."
        $ splitPath
        $ dropExtension fin
  info verbosity $ "erb-processing " ++ fin' ++ " to " ++ fout'
  setEnv "ERB_HS_MODULE" modName
  readProcess "erb" [fin'] "" >>= writeFile fout'
