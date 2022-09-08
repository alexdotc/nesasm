module Main where

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Header

data InesFile = InesFile { 
                  header  :: InesHeader
                , trainer :: InesTrainer
                , prgrom  :: InesPRGROM
                , chrrom  :: InesCHRROM
                , miscrom :: InesMiscROM
                } deriving Show

type InesTrainer = ByteString
type InesPRGROM = ByteString
type InesCHRROM = ByteString
type InesMiscROM = ByteString

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do putStrLn "Please specify a NES ROM!"; exitFailure
    _ -> return ()
  nesh <- BL.readFile $ head args
  print $ runGet readInesHeader nesh
