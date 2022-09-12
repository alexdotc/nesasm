module Opcode (Opcode, parseOpcode) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import Data.Binary.Get
import Data.Bits
import Data.Word (Word8, Word16, Word32)

data Opcode = Op Instruction AddressingMode deriving (Eq, Show)

data AddressingMode =  Implied
                     | Accumulator
                     | Immediate
                     | ZeroPage
                     | Absolute
                     | Relative
                     | Indirect
                     | ZeroPageIndexedX
                     | ZeroPageIndexedY
                     | AbsoluteIndexedX
                     | AbsoluteIndexedY
                     | IndexedIndirect
                     | IndirectIndexed
                     deriving (Eq, Show)

data Instruction =  BRK | ORA | SLO | NOP | ASL | ANC | BPL | CLC | SED
                  | JSR | BIT | ROL | RLA | BMI | SEC | AND | RTI | EOR
                  | SRE | LSR | PHA | PLP | PHP | ALR | JMP | BVC | CLI
                  | RTS | ADC | RRA | ROR | PLA | ARR | BVS | SEI | STA
                  | SAX | STY | STX | DEY | TXA | XAA | BCC | AHX | TYA
                  | TXS | TAS | SHY | SHX | LDY | LDA | LDX | LAX | TAY
                  | BCS | CLV | TSX | LAS | CPY | CMP | DCP | DEC | INY
                  | DEX | AXS | BNE | CLD | CPX | ISC | SBC | INC | BEQ
                  deriving (Eq, Show)

parseOpcode :: Word8 -> Opcode
parseOpcode b = case b of
  0x00 -> Op BRK Implied
  0x01 -> Op ORA IndexedIndirect
  0x03 -> Op SLO IndexedIndirect
  0x04 -> Op NOP ZeroPage
  0x05 -> Op ORA ZeroPage
  0x06 -> Op ASL ZeroPage
  0x07 -> Op SLO ZeroPage
  0x08 -> Op PHP Implied
  0x09 -> Op ORA Immediate
  0x0A -> Op ASL Accumulator
  0x0B -> Op ANC Immediate
  0x0C -> Op NOP Absolute
  0x0D -> Op ORA Absolute
  0x0E -> Op ASL Absolute
  0x0F -> Op SLO Absolute
  0x10 -> Op BPL Relative
  0x11 -> Op ORA IndirectIndexed
  0x13 -> Op SLO IndirectIndexed
  0x14 -> Op NOP ZeroPageIndexedX
  0x15 -> Op ORA ZeroPageIndexedX
  0x16 -> Op ASL ZeroPageIndexedX
  0x17 -> Op SLO ZeroPageIndexedX
  0x18 -> Op CLC Implied
  0x19 -> Op ORA AbsoluteIndexedY
  0x1A -> Op NOP Implied
  0x1B -> Op SLO AbsoluteIndexedY
  0x1C -> Op NOP AbsoluteIndexedX
  0x1D -> Op ORA AbsoluteIndexedX
  0x1E -> Op ASL AbsoluteIndexedX
  0x1F -> Op SLO AbsoluteIndexedX
  0x20 -> Op JSR Absolute
  0x21 -> Op AND IndexedIndirect
  0x23 -> Op RLA IndexedIndirect
  0x24 -> Op BIT ZeroPage
  0x25 -> Op AND ZeroPage
  0x26 -> Op ROL ZeroPage
  0x27 -> Op RLA ZeroPage
  0x28 -> Op PLP Implied
  0x29 -> Op AND Immediate
  0x2A -> Op ROL Accumulator
  0x2B -> Op ANC Immediate
  0x2C -> Op BIT Absolute
  0x2D -> Op AND Absolute
  0x2E -> Op ROL Absolute
  0x2F -> Op RLA Absolute
  0x30 -> Op BMI Relative
  0x31 -> Op AND IndirectIndexed
  0x33 -> Op RLA IndirectIndexed
  0x34 -> Op NOP ZeroPageIndexedX
  0x35 -> Op AND ZeroPageIndexedX
  0x36 -> Op ROL ZeroPageIndexedX
  0x37 -> Op RLA ZeroPageIndexedX
  0x38 -> Op SEC Implied
  0x39 -> Op AND AbsoluteIndexedY
  0x3A -> Op NOP Implied
  0x3B -> Op RLA AbsoluteIndexedY
  0x3C -> Op NOP AbsoluteIndexedX
  0x3D -> Op AND AbsoluteIndexedX
  0x3E -> Op ROL AbsoluteIndexedX
  0x3F -> Op RLA AbsoluteIndexedX
  0x40 -> Op RTI Implied
  0x41 -> Op EOR IndexedIndirect
  0x43 -> Op SRE IndexedIndirect
  0x44 -> Op NOP ZeroPage
  0x45 -> Op EOR ZeroPage
  0x46 -> Op LSR ZeroPage
  0x47 -> Op SRE ZeroPage
  0x48 -> Op PHA Implied
  0x49 -> Op EOR Immediate
  0x4A -> Op LSR Accumulator
  0x4B -> Op ALR Immediate
  0x4C -> Op JMP Absolute
  0x4D -> Op EOR Absolute
  0x4E -> Op LSR Absolute
  0x4F -> Op SRE Absolute
  0x50 -> Op BVC Relative
  0x51 -> Op EOR IndirectIndexed
  0x53 -> Op SRE IndirectIndexed
  0x54 -> Op NOP ZeroPageIndexedX
  0x55 -> Op EOR ZeroPageIndexedX
  0x56 -> Op LSR ZeroPageIndexedX
  0x57 -> Op SRE ZeroPageIndexedX
  0x58 -> Op CLI Implied
  0x59 -> Op EOR AbsoluteIndexedY
  0x5A -> Op NOP Implied
  0x5B -> Op SRE AbsoluteIndexedY
  0x5C -> Op NOP AbsoluteIndexedX
  0x5D -> Op EOR AbsoluteIndexedX
  0x5E -> Op LSR AbsoluteIndexedX
  0x5F -> Op SRE AbsoluteIndexedX
  0x60 -> Op RTS Implied
  0x61 -> Op ADC IndexedIndirect
  0x63 -> Op RRA IndexedIndirect
  0x64 -> Op NOP ZeroPage
  0x65 -> Op ADC ZeroPage
  0x66 -> Op ROR ZeroPage
  0x67 -> Op RRA ZeroPage
  0x68 -> Op PLA Implied
  0x69 -> Op ADC Immediate
  0x6A -> Op ROR Accumulator
  0x6B -> Op ARR Immediate
  0x6C -> Op JMP Indirect
  0x6D -> Op ADC Absolute
  0x6E -> Op ROR Absolute
  0x6F -> Op RRA Absolute
  0x70 -> Op BVS Relative
  0x71 -> Op ADC IndirectIndexed
  0x73 -> Op RRA IndirectIndexed
  0x74 -> Op NOP ZeroPageIndexedX
  0x75 -> Op ADC ZeroPageIndexedX
  0x76 -> Op ROR ZeroPageIndexedX
  0x77 -> Op RRA ZeroPageIndexedX
  0x78 -> Op SEI Implied
  0x79 -> Op ADC AbsoluteIndexedY
  0x7A -> Op NOP Implied
  0x7B -> Op RRA AbsoluteIndexedY
  0x7C -> Op NOP AbsoluteIndexedY
  0x7D -> Op ADC AbsoluteIndexedY
  0x7E -> Op ROR AbsoluteIndexedY
  0x7F -> Op RRA AbsoluteIndexedY
