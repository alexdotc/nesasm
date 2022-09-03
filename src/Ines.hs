{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word16, Word32)
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
                  , mapper :: Word16 -- 12-bit mapper number
                  , submapper :: Word8 -- offset 8 bits 4-7 submapper
                  , prgramsize :: Word32 -- offset 10 PRG-RAM size
                  , prgnvramsize :: Word32 -- offset 10 PRG-NVRAM/EEPROM size
                  , chrramsize :: Word32 -- offset 11 CHR-RAM size
                  , chrnvramsize :: Word32 -- offset 11 CHR-NVRAM size
                  , timing     :: Timing -- offset 12 CPU/PPU timings
                  , miscroms   :: Word8 -- offset 14 Misc ROMs Present
                  , expdev     :: Word8 -- offset 15 Default Expansion Device
                  , battery    :: Bool -- Battery or non-volatile memory
                  , trainer512 :: Bool -- 512 Byte Trainer
                  , hw4screenmode :: Bool -- Hard-wired four-screen mode
                  , nes2       :: Bool -- NES2.0 extended format ROM?
                  , hwntm       :: HWNTM -- Hard-wried nametable mirroring type
                  , consoleType :: ConsoleType -- Regular NES or special?
                  , vsSystemType :: VsSystemType -- VS System Type if VS System
                  } deriving Show

type InesTrainer = ByteString
type InesPRGROM = ByteString
type InesCHRROM = ByteString
type InesMiscROM = ByteString

readInesHeader :: Get InesHeader
readInesHeader = do
  getLazyByteString 4 -- TODO validate "NES\SUB" ... do Word32 instead?
  (
    prgLSB:
    chrLSB:
    flags6:
    flags7:
    mapperMSB:
    prgchrMSBs:
    prgramshifts:
    chrramshifts:
    tmng:
    b13:
    mscroms:
    expdv:
    _) <- replicateM 12 getWord8
  let prgromsize = word12PRG prgLSB prgchrMSBs
  let chrromsize = word12CHR chrLSB prgchrMSBs
  let (hwntm, battery, trainer512, hw4screenmode, m03) = parseFlags6 flags6
  let (nes2, consoleType, m47) = parseFlags7 flags7 b13
  let (mapper, submapper) = mapperSubmapper m03 m47 mapperMSB
  let (prgramsize, prgnvramsize) = parseInesShifts prgramshifts
  let (chrramsize, chrnvramsize) = parseInesShifts chrramshifts
  let timing = parseTiming tmng
  let miscroms = mscroms .&. 0x03
  let expdev = expdv .&. 0xC0
  return $ InesHeader{..}

parseInesShifts :: Word8 -> (Word32, Word32)
parseInesShifts b = (f n1, f n2) where
  f :: Word8 -> Word32
  f n = let x = shiftL 64 (fromIntegral n) in if n == 0 then 0 else x
  n1 = b .&. 0x0F
  n2 = shiftR b 4 .&. 0x0F

-- get mapper and submapper from nibbles
mapperSubmapper :: Word8 -> Word8 -> Word8 -> (Word16, Word8)
mapperSubmapper m03 m47 msb = (mapper, submapper)
  where submapper = shiftR msb 4
        mapper = (fromIntegral (m03 .|. m47) :: Word16) .|. (shiftL (fromIntegral msb :: Word16) 8 .&. 0x0F00)

-- fuse LSB and right nibble MSB for 12-bit PRG ROM size
word12PRG :: Word8 -> Word8 -> Word16 
word12PRG lsb msb = shiftL (fromIntegral msb :: Word16) 8 .&. 0x0F00 .|. (fromIntegral lsb)

-- fuse LSB and left nibble MSB for 12-bit CHR ROM size
word12CHR :: Word8 -> Word8 -> Word16 
word12CHR lsb msb = shiftL (fromIntegral msb :: Word16) 4 .&. 0x0F00 .|. (fromIntegral lsb)

-- Header Byte 6 Flags
type Battery = Bool
type Trainer512B = Bool
-- Hard-wired four-screen mode
type HWFourScreenMode = Bool
-- Hard-wried nametable mirroring type
data HWNTM = HorizontalOrMapperControlled | Vertical deriving (Eq, Show)

-- Header Byte 7 Flags
type NES2_0 = Bool
type MapperBits = Word8
data ConsoleType = NESFamicom | VsSystem VSType | Playchoice10 | ExtendedConsole ECType deriving (Eq, Show)
data ECType =   DecModeCPU -- Regular Famiclone, but with CPU that supports Decimal Mode
             |  EPSM -- Regular NES/Famicom with EPSM module or plug-through cartridge
             |  VT01  -- V.R. Technology VT01 with red/cyan STN palette
             |  VT02  -- V.R. Technology VT02
             |  VT03  -- V.R. Technology VT03
             |  VT09  -- V.R. Technology VT09
             |  VT32  -- V.R. Technology VT32
             |  VT369 -- V.R. Technology VT369
             |  UMCUM6578
             |  FamicomNetworkSystem
             deriving (Eq, Show) 

data VSType = Placeholder deriving (Eq, Show)

parseFlags6 :: Word8 -> (HWNTM, Battery, Trainer512B, HWFourScreenMode, MapperBits)
parseFlags6 b b13 = (ntm,ba,bt,sm4,m)
  where ntm = if b .&. 0b1 /= 0 then Vertical else HorizontalOrMapperControlled
        ba = if b .&. 0b10 /= 0 then True else False
        bt = if b .&. 0b100 /= 0 then True else False
        sm4 = if b .&. 0b1000 /= 0 then True else False
        m = shiftR b 4

parseFlags7 :: Word8 -> Word8 -> (NES2_0, ConsoleType, MapperBits)
parseFlags7 b b13 = (n,c,m)
  where n = b .&. 0b1100 == 0b1000
        c = case b .&. 0b11 of
              0b00 -> NESFamicom
              0b01 -> VsSystem $ parseVST b13
              0b10 -> Playchoice10
              0b11 -> ExtendedConsole $ parseECT b13
        m = b .&. 0xF0

parseECT :: Word8 -> ECType
parseECT b = FamicomNetworkSystem --TODO

parseVST :: Word8 -> VSType
parseVST b = Placeholder -- TODO

-- CPU/PPU Timing
data Timing = RP2C02 | RP2C07 | Multiregion | UMC6527P deriving (Eq, Show)

parseTiming :: Word8 -> Timing
parseTiming b = case b .&. 0b11 of
                  0b00 -> RP2C02
                  0b01 -> RP2C07
                  0b10 -> Multiregion
                  0b11 -> UMC6527P
 
main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do putStrLn "Please specify a NES ROM!"; exitFailure
    _ -> return ()
  nesh <- BL.readFile $ head args
  print $ runGet readInesHeader nesh
