module Opcode (Opcode, parseOpcode) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import Data.Binary.Get
import Data.Bits
import Data.Word (Word8, Word16, Word32)

data Opcode = Op Instruction AddressingMode deriving (Eq, Show)

data AddressingMode =  Implied
                     | Accumulator
                     | Immeidate
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

data Instruction =  BRK | ORA | STP | SLO | NOP | ASL | ANC | BPL | CLC
                  | JSR | BIT | ROL | RLA | BMI | SEC | AND | RTI | EOR
                  | SRE | LSR | PHA | PLP | PHP | ALR | JMP | BVC | CLI
                  | RTS | ADC | RRA | ROR | PLA | ARR | BVS | SEI | STA
                  | SAX | STY | STX | DEY | TXA | XAA | BCC | AHX | TYA
                  | TXS | TAS | SHY | SHX | LDY | LDA | LDX | LAX | TAY
                  | BCS | CLV | TSX | LAS | CPY | CMP | DCP | DEC | INY
                  | DEX | AXS | BNE | CLD | CPX | ISC | SBC | INC | BEQ
                  | SED deriving (Eq, Show)

parseOpcode :: Word8 -> Opcode
parseOpcode b = undefined
