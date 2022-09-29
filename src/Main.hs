{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Binary.Get as G
import           Data.Binary.Get (Get)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import Header
import Disassemble

type InesROM = ByteString
type InesTrainer = Maybe ByteString
type InesMiscROM = Maybe ByteString

data InesFile = InesFile { 
                  header  :: InesHeader
                , trainer :: InesTrainer
                , prgrom  :: InesROM
                , chrrom  :: InesROM
                , miscrom :: InesMiscROM
                } deriving Show

getROM :: Integral a => a -> Get InesROM
getROM size = G.getLazyByteString $ fromIntegral size

chunkrom :: Get InesFile
chunkrom = do
  header  <- getInesHeader
  trainer <- case hasTrainer header of
                True -> return <$> getROM 512
                False -> return Nothing
  prgrom  <- getROM $ prgromsize header
  chrrom  <- getROM $ chrromsize header
  miscrom <- case miscroms header of
                0 -> return Nothing
                _ -> return <$> let mr = miscroms header in getROM mr
  return $ InesFile{..}

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do putStrLn "Please specify a NES ROM!"; exitFailure
    _ -> return ()
  nes2_0 <- B.readFile $ head args
  let inr =  G.runGet chunkrom nes2_0
  print $ disassemble $ B.toStrict $ prgrom inr
