module Disassemble where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)

import Opcode

type InesTrainer = Maybe ByteString
type InesPRGROM = [(Mnemonic, Operand)]
type InesCHRROM = ByteString
type InesMiscROM = Maybe ByteString

type Operand = Maybe ByteString

disassemble :: Get InesPRGROM
disassemble = undefined
