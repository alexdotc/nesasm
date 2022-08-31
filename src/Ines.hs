{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word16)

data InesFile = InesFile { 
                  header  :: InesHeader
                , trainer :: InesTrainer
                , prgrom  :: InesPRGROM
                , chrrom  :: InesCHRROM
                , miscrom :: InesMiscROM
                } deriving Show

data InesHeader = InesHeader { 
                    prgromsize :: Word16 -- offset 4 (LSB), 9 bits 0-3 (MSB) 16K PRG ROM pages
                  , chrromsize :: Word16 -- offset 5 (LSB), 9 bits 4-7 (MSB) 8K CHR ROM pages
                  , flags6 :: Word8 -- offset 6 flags
                  , flags7 :: Word8 -- offset 7 flags
                  , mapper :: Word8 -- offset 8 mapper info
                  , prgramsize :: Word8 -- offset 10 PRG-RAM/EEPROM size
                  , chrramsize :: Word8 -- offset 11
                  , timing     :: Word8 -- offset 12 CPU/PPU timings
                  , vssystem   :: Word8 -- offset 13 Vs. System Type or Extended Console Type
                  , miscroms   :: Word8 -- offset 14 Misc ROMs Present
                  , expdev     :: Word8 -- offset 15 Default Expansion Device
                  } deriving Show

type InesTrainer = ByteString
type InesPRGROM = ByteString
type InesCHRROM = ByteString
type InesMiscROM = ByteString
type NesID = (ByteString, Word16)

-- fuse LSB and right nibble MSB for 12-bit PRG ROM size
word12PRG :: Word8 -> Word8 -> Word16 
word12PRG lsb msb = shift (fromIntegral msb :: Word16) 8 .&. 0x0F00 .|. (fromIntegral lsb)

-- fuse LSB and left nibble MSB for 12-bit CHR ROM size
word12CHR :: Word8 -> Word8 -> Word16 
word12CHR lsb msb = shift (fromIntegral msb :: Word16) 4 .&. 0x0F00 .|. (fromIntegral lsb)

readInesFile :: Get InesHeader
readInesFile = do
  nesid <- getLazyByteString 4 -- TODO validate "NES\SUB" ... do Word32 instead?
  prgLSB <- getWord8
  chrLSB <- getWord8
  flgs6 <- getWord8
  flgs7 <- getWord8
  mapper <- getWord8
  prgchrMSBs <- getWord8
  prgram <- getWord8
  chrram <- getWord8
  tmng <- getWord8
  vsec <- getWord8
  miscr <- getWord8
  expdev <- getWord8
  let prgrom = word12PRG prgLSB prgchrMSBs
  let chrrom = word12CHR chrLSB prgchrMSBs
  return $ InesHeader prgrom chrrom flgs6 flgs7 mapper prgram chrram tmng vsec miscr expdev
 
main :: IO ()
main = do
  nesh <- BL.readFile "ex.nes"
  print $ runGet readInesFile nesh
