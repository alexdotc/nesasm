{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Header (InesHeader, readInesHeader) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import Data.Binary.Get
import Data.Bits
import Data.Word (Word8, Word16, Word32)

data InesHeader = InesHeader { 
                    prgromsize :: Word16 -- offset 4 (LSB), 9 bits 0-3 (MSB) 16K PRG ROM pages
                  , chrromsize :: Word16 -- offset 5 (LSB), 9 bits 4-7 (MSB) 8K CHR ROM pages
                  , mapper :: Word16 -- 12-bit mapper number
                  , submapper :: Word8 -- offset 8 bits 4-7 submapper
                  , prgramsize :: Word32 -- offset 10 PRG-RAM size
                  , prgnvramsize :: Word32 -- offset 10 PRG-NVRAM/EEPROM size
                  , chrramsize :: Word32 -- offset 11 CHR-RAM size
                  , chrnvramsize :: Word32 -- offset 11 CHR-NVRAM size
                  , miscroms   :: Word8 -- offset 14 Misc ROMs Present
                  , timing     :: Timing -- offset 12 CPU/PPU timings
                  , expdev     :: ExpDev -- offset 15 Default Expansion Device
                  , battery    :: Bool -- Battery or non-volatile memory
                  , trainer512 :: Bool -- 512 Byte Trainer
                  , hw4screenmode :: Bool -- Hard-wired four-screen mode
                  , nes2       :: Bool -- NES2.0 extended format ROM?
                  , hwntm       :: HWNTM -- Hard-wried nametable mirroring type
                  , consoleType :: ConsoleType -- Vs/Extended/Regular
                  } deriving Show

readInesHeader :: Get InesHeader
readInesHeader = do
  getLazyByteString 4 -- TODO validate "NES\SUB" ... do Word32 instead?
  (b4:b5:b6:b7:b8:b9:b10:b11:b12:b13:b14:b15:_) <- replicateM 12 getWord8

  let prgromsize = word12PRG b4 b9
  let chrromsize = word12CHR b5 b9
  let (hwntm, battery, trainer512, hw4screenmode, m03) = parseFlags6 b6
  let (nes2, consoleType, m47) = parseFlags7 b7 b13
  let (mapper, submapper) = mapperSubmapper m03 m47 b8
  let (prgramsize, prgnvramsize) = parseInesShifts b10
  let (chrramsize, chrnvramsize) = parseInesShifts b11
  let timing = parseTiming b12
  let miscroms = b14 .&. 0x03
  let expdev = parseExpDev b15

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
data ConsoleType = NESFamicom | VsSystem VsPPUType VsHWType | Playchoice10 | ExtendedConsole ECType deriving (Eq, Show)
data ECType =  DecModeCPU -- Regular Famiclone, but with CPU that supports Decimal Mode
             | EPSM -- Regular NES/Famicom with EPSM module or plug-through cartridge
             | VT01  -- V.R. Technology VT01 with red/cyan STN palette
             | VT02  -- V.R. Technology VT02
             | VT03  -- V.R. Technology VT03
             | VT09  -- V.R. Technology VT09
             | VT32  -- V.R. Technology VT32
             | VT369 -- V.R. Technology VT369
             | UMCUM6578
             | FamicomNetworkSystem
             | ReservedOrUnknownECT
             deriving (Eq, Show) 

data VsPPUType =   RP2C03B
                 | RP2C03G
                 | RP2C04_0001
                 | RP2C04_0002
                 | RP2C04_0003
                 | RP2C04_0004
                 | RC2C03B
                 | RC2C03C
                 | RC2C05_01
                 | RC2C05_02
                 | RC2C05_03
                 | RC2C05_04
                 | RC2C05_05
                 | ReservedOrUnknownVSPPUType
                 deriving (Eq, Show)

data VsHWType =   VsUnisystemNormal -- Vs. Unisystem (normal)
                | VsUnisystemProtRBIBaseball -- Vs. Unisystem (RBI Baseball protection)
                | VsUnisystemProtTKOBoxing -- Vs. Unisystem (TKO Boxing protection)
                | VsUnisystemProtSuperXevious -- Vs. Unisystem (Super Xevious protection)
                | VsUnisystemProtIceClimber -- Vs. Unisystem (Vs. Ice Climber Japan protection)
                | VsDualsystemNormal -- Vs. Dual System (normal)
                | VsDualsystemProtROBB -- Vs. Dual System (Raid on Bungeling Bay protection)
                | ReservedOrUnknownVSHWType
                deriving (Eq, Show)

parseFlags6 :: Word8 -> (HWNTM, Battery, Trainer512B, HWFourScreenMode, MapperBits)
parseFlags6 b = (ntm,ba,bt,sm4,m)
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
              0b01 -> runReader parseVST b13
              0b10 -> Playchoice10
              0b11 -> ExtendedConsole $ parseECT b13
        m = b .&. 0xF0

parseECT :: Word8 -> ECType
parseECT b = case b .&. 0xF of
               0x3 -> DecModeCPU
               0x4 -> EPSM
               0x5 -> VT01
               0x6 -> VT02
               0x7 -> VT03
               0x8 -> VT09
               0x9 -> VT32
               0xA -> VT369
               0xB -> UMCUM6578
               0xC -> FamicomNetworkSystem
               _   -> ReservedOrUnknownECT

parseVST :: Reader Word8 ConsoleType
parseVST = reader $ \b ->
             let p = case b .&. 0xF of
                       0x0 -> RP2C03B
                       0x1 -> RP2C03G
                       0x2 -> RP2C04_0001
                       0x3 -> RP2C04_0002
                       0x4 -> RP2C04_0003
                       0x5 -> RP2C04_0004
                       0x6 -> RC2C03B
                       0x7 -> RC2C03C
                       0x8 -> RC2C05_01
                       0x9 -> RC2C05_02
                       0xA -> RC2C05_03
                       0xB -> RC2C05_04
                       0xC -> RC2C05_05
                       _   -> ReservedOrUnknownVSPPUType
                 h = case shiftR b 4 of
                       0x0 -> VsUnisystemNormal
                       0x1 -> VsUnisystemProtRBIBaseball
                       0x2 -> VsUnisystemProtTKOBoxing
                       0x3 -> VsUnisystemProtSuperXevious
                       0x4 -> VsUnisystemProtIceClimber
                       0x5 -> VsDualsystemNormal
                       0x6 -> VsDualsystemProtROBB
                       _   -> ReservedOrUnknownVSHWType
             in VsSystem p h

data ExpDev =  Unspecified -- https://www.nesdev.org/wiki/NES_2.0#Default_Expansion_Device
             | StandardControllers
             | NESFourScoreSatellite
             | FamicomFourPlayersAdapter
             | VsSystem1P4016
             | VsSystem1P4017
             | VsZapper
             | Zapper
             | TwoZappers
             | BandaiHyperShotLightgun
             | PowerPadSideA
             | PowerPadSideB
             | FamilyTrainerSideA
             | FamilyTrainerSideB
             | ArkanoidVausControllerNES
             | ArkanoidVausControllerFamicom
             | TwoVausControllersPlusFamicomDataRecorder
             | KonamiHyperShotController
             | CoconutsPachinkoController
             | ExcitingBoxingPunchingBagBlowupDoll
             | JissenMahjongController
             | PartyTap
             | OekaKidsTablet
             | SunsoftBarcodeBattler
             | MiraclePianoKeyboard
             | PokkunMoguraaWhackAMoleMatandMallet
             | TopRiderInflatableBicycle
             | DoubleFisted
             | Famicom3DSystem
             | DoremikkoKeyboard
             | ROBGyroSet
             | FamicomDataRecorder
             | ASCIITurboFile
             | IGSStorageBattleBox
             | FamilyBASICKeyboardPlusFamicomDataRecorder
             | DongdaPEC586Keyboard
             | BitCorpBit79Keyboard
             | SuborKeyboard
             | SuborKeyboardMouse3x8
             | SuborKeyboardMouse24
             | SNESMouse
             | Multicart
             | TwoSNEScontrollers
             | RacerMateBicycle
             | UForce
             | ROBStackUp
             | CityPatrolmanLightgun
             | SharpC1CassetteInterface
             | StandardControllerSwappedDPadBA
             | ExcaliborSudokuPad
             | ABLPinball
             | GoldenNuggetCasinoExtraButtons
             | ReservedOrUnknownExpDev
            deriving (Eq, Show)

parseExpDev :: Word8 -> ExpDev
parseExpDev b = case b .&. 0x3F of
                  0x00 -> Unspecified
                  0x01 -> StandardControllers
                  0x02 -> NESFourScoreSatellite
                  0x03 -> FamicomFourPlayersAdapter
                  0x04 -> VsSystem1P4016
                  0x05 -> VsSystem1P4017
                  -- 0x06 -> Reserved
                  0x07 -> VsZapper
                  0x08 -> Zapper
                  0x09 -> TwoZappers
                  0x0A -> BandaiHyperShotLightgun
                  0x0B -> PowerPadSideA
                  0x0C -> PowerPadSideB
                  0x0D -> FamilyTrainerSideA
                  0x0E -> FamilyTrainerSideB
                  0x0F -> ArkanoidVausControllerNES
                  0x10 -> ArkanoidVausControllerFamicom
                  0x11 -> TwoVausControllersPlusFamicomDataRecorder
                  0x12 -> KonamiHyperShotController
                  0x13 -> CoconutsPachinkoController
                  0x14 -> ExcitingBoxingPunchingBagBlowupDoll
                  0x15 -> JissenMahjongController
                  0x16 -> PartyTap
                  0x17 -> OekaKidsTablet
                  0x18 -> SunsoftBarcodeBattler
                  0x19 -> MiraclePianoKeyboard
                  0x1A -> PokkunMoguraaWhackAMoleMatandMallet
                  0x1B -> TopRiderInflatableBicycle
                  0x1C -> DoubleFisted
                  0x1D -> Famicom3DSystem
                  0x1E -> DoremikkoKeyboard
                  0x1F -> ROBGyroSet
                  0x20 -> FamicomDataRecorder
                  0x21 -> ASCIITurboFile
                  0x22 -> IGSStorageBattleBox
                  0x23 -> FamilyBASICKeyboardPlusFamicomDataRecorder
                  0x24 -> DongdaPEC586Keyboard
                  0x25 -> BitCorpBit79Keyboard
                  0x26 -> SuborKeyboard
                  0x27 -> SuborKeyboardMouse3x8
                  0x28 -> SuborKeyboardMouse24
                  0x29 -> SNESMouse
                  0x2A -> Multicart
                  0x2B -> TwoSNEScontrollers
                  0x2C -> RacerMateBicycle
                  0x2D -> UForce
                  0x2E -> ROBStackUp
                  0x2F -> CityPatrolmanLightgun
                  0x30 -> SharpC1CassetteInterface
                  0x31 -> StandardControllerSwappedDPadBA
                  0x32 -> ExcaliborSudokuPad
                  0x33 -> ABLPinball
                  0x34 -> GoldenNuggetCasinoExtraButtons
                  _    -> ReservedOrUnknownExpDev
                         

-- CPU/PPU Timing
data Timing = RP2C02 | RP2C07 | Multiregion | UMC6527P deriving (Eq, Show)

parseTiming :: Word8 -> Timing
parseTiming b = case b .&. 0b11 of
                  0b00 -> RP2C02
                  0b01 -> RP2C07
                  0b10 -> Multiregion
                  0b11 -> UMC6527P
