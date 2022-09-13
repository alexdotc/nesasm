{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Binary.Get
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

disassemble :: Get InesFile
disassemble = do
  header  <- readInesHeader
  prgrom  <- readPRGROM
  chrrom  <- getLazyByteString 16
  let trainer = Nothing
  let miscrom = Nothing
  return $ InesFile{..}

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do putStrLn "Please specify a NES ROM!"; exitFailure
    _ -> return ()
  nesh <- BL.readFile $ head args
  print $ runGet disassemble nesh
