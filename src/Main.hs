module Main where

import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Header
import Disassemble

data InesFile = InesFile { 
                  header  :: InesHeader
                , trainer :: InesTrainer
                , prgrom  :: InesPRGROM
                , chrrom  :: InesCHRROM
                , miscrom :: InesMiscROM
                } deriving Show

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do putStrLn "Please specify a NES ROM!"; exitFailure
    _ -> return ()
  nesh <- BL.readFile $ head args
  print $ runGet readInesHeader nesh
