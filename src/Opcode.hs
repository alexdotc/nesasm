module Opcode where

import Data.Word (Word8)

data Opcode = Op Mnemonic AddressingMode deriving (Eq, Show)

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

data Mnemonic =  BRK | ORA | SLO | NOP | ASL | ANC | BPL | CLC | SED
                  | JSR | BIT | ROL | RLA | BMI | SEC | AND | RTI | EOR
                  | SRE | LSR | PHA | PLP | PHP | ALR | JMP | BVC | CLI
                  | RTS | ADC | RRA | ROR | PLA | ARR | BVS | SEI | STA
                  | SAX | STY | STX | DEY | TXA | XAA | BCC | AHX | TYA
                  | TXS | TAS | SHY | SHX | LDY | LDA | LDX | LAX | TAY
                  | BCS | CLV | TSX | LAS | CPY | CMP | DCP | DEC | INY
                  | DEX | AXS | BNE | CLD | CPX | ISC | SBC | INC | BEQ
                  | INX | TAX | KIL
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
  0x80 -> Op NOP Immediate
  0x81 -> Op STA IndexedIndirect
  0x82 -> Op NOP Immediate
  0x83 -> Op RRA IndexedIndirect
  0x84 -> Op STY ZeroPage
  0x85 -> Op STA ZeroPage
  0x86 -> Op STX ZeroPage
  0x87 -> Op SAX ZeroPage
  0x88 -> Op DEY Implied
  0x89 -> Op NOP Immediate
  0x8A -> Op TXA Implied
  0x8B -> Op XAA Immediate
  0x8C -> Op STY Absolute
  0x8D -> Op STA Absolute
  0x8E -> Op STX Absolute
  0x8F -> Op SAX Absolute
  0x90 -> Op BCC Relative
  0x91 -> Op STA IndirectIndexed
  0x93 -> Op AHX IndirectIndexed
  0x94 -> Op STY ZeroPageIndexedX
  0x95 -> Op STA ZeroPageIndexedX
  0x96 -> Op STX ZeroPageIndexedY
  0x97 -> Op SAX ZeroPageIndexedY
  0x98 -> Op TYA Implied
  0x99 -> Op STA AbsoluteIndexedY
  0x9A -> Op TXS Implied
  0x9B -> Op TAS AbsoluteIndexedY
  0x9C -> Op SHY AbsoluteIndexedX
  0x9D -> Op STA AbsoluteIndexedX
  0x9E -> Op SHX AbsoluteIndexedY
  0x9F -> Op AHX AbsoluteIndexedY
  0xA0 -> Op LDY Immediate
  0xA1 -> Op LDA IndexedIndirect
  0xA2 -> Op LDX Immediate
  0xA3 -> Op LAX IndexedIndirect
  0xA4 -> Op LDY ZeroPage
  0xA5 -> Op LDA ZeroPage
  0xA6 -> Op LDX ZeroPage
  0xA7 -> Op LAX ZeroPage
  0xA8 -> Op TAY Implied
  0xA9 -> Op LDA Immediate
  0xAA -> Op TAX Implied
  0xAB -> Op LAX Immediate
  0xAC -> Op LDY Absolute
  0xAD -> Op LDA Absolute
  0xAE -> Op LDX Absolute
  0xAF -> Op LAX Absolute
  0xB0 -> Op BCS Relative
  0xB1 -> Op LDA IndirectIndexed
  0xB3 -> Op LAX IndirectIndexed
  0xB4 -> Op LDY ZeroPageIndexedX
  0xB5 -> Op LDA ZeroPageIndexedX
  0xB6 -> Op LDX ZeroPageIndexedY
  0xB7 -> Op LAX ZeroPageIndexedY
  0xB8 -> Op CLV Implied
  0xB9 -> Op LDA AbsoluteIndexedY
  0xBA -> Op TSX Implied
  0xBB -> Op LAS AbsoluteIndexedY
  0xBC -> Op LDY AbsoluteIndexedX
  0xBD -> Op LDA AbsoluteIndexedX
  0xBE -> Op LDX AbsoluteIndexedY
  0xBF -> Op LAX AbsoluteIndexedY
  0xC0 -> Op CPY Immediate
  0xC1 -> Op CMP IndexedIndirect
  0xC2 -> Op NOP Immediate
  0xC3 -> Op DCP IndexedIndirect
  0xC4 -> Op CPY ZeroPage
  0xC5 -> Op CMP ZeroPage
  0xC6 -> Op DEC ZeroPage
  0xC7 -> Op DCP ZeroPage
  0xC8 -> Op INY Implied
  0xC9 -> Op CMP Immediate
  0xCA -> Op DEX Implied
  0xCB -> Op AXS Immediate
  0xCC -> Op CPY Absolute
  0xCD -> Op CMP Absolute
  0xCE -> Op DEC Absolute
  0xCF -> Op DCP Absolute
  0xD0 -> Op BNE Relative
  0xD1 -> Op CMP IndirectIndexed
  0xD3 -> Op DCP IndirectIndexed
  0xD4 -> Op NOP ZeroPageIndexedX
  0xD5 -> Op CMP ZeroPageIndexedX
  0xD6 -> Op DEC ZeroPageIndexedX
  0xD7 -> Op DCP ZeroPageIndexedX
  0xD8 -> Op CLD Implied
  0xD9 -> Op CMP AbsoluteIndexedY
  0xDA -> Op NOP Implied
  0xDB -> Op DCP AbsoluteIndexedY
  0xDC -> Op NOP AbsoluteIndexedX
  0xDD -> Op CMP AbsoluteIndexedX
  0xDE -> Op DEC AbsoluteIndexedX
  0xDF -> Op DCP AbsoluteIndexedX
  0xE0 -> Op CPX Immediate
  0xE1 -> Op SBC IndexedIndirect
  0xE2 -> Op NOP Immediate
  0xE3 -> Op ISC IndexedIndirect
  0xE4 -> Op CPX ZeroPage
  0xE5 -> Op SBC ZeroPage
  0xE6 -> Op INC ZeroPage
  0xE7 -> Op ISC ZeroPage
  0xE8 -> Op INX Implied
  0xE9 -> Op SBC Immediate
  0xEA -> Op NOP Implied
  0xEB -> Op SBC Immediate
  0xEC -> Op CPX Absolute
  0xED -> Op SBC Absolute
  0xEE -> Op INC Absolute
  0xEF -> Op ISC Absolute
  0xF0 -> Op BEQ Relative
  0xF1 -> Op SBC IndirectIndexed
  0xF3 -> Op ISC IndirectIndexed
  0xF4 -> Op NOP ZeroPageIndexedX
  0xF5 -> Op SBC ZeroPageIndexedX
  0xF6 -> Op INC ZeroPageIndexedX
  0xF7 -> Op ISC ZeroPageIndexedX
  0xF8 -> Op SED Implied
  0xF9 -> Op SBC AbsoluteIndexedY
  0xFA -> Op NOP Implied
  0xFB -> Op ISC AbsoluteIndexedY
  0xFC -> Op NOP AbsoluteIndexedX
  0xFD -> Op SBC AbsoluteIndexedX
  0xFE -> Op INC AbsoluteIndexedX
  0xFF -> Op ISC AbsoluteIndexedX
  _    -> Op KIL Implied -- TODO actually stop the parse
