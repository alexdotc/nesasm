module Disassemble where

import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString.Lazy (ByteString)
import Data.Word

import Opcode

data DisasmState = DisasmState {
                     position :: Word32
                   , done :: [Instruction]
                   , stack :: [Word32]
                   , prg :: ByteString
                   } deriving Show

type Disassembler = StateT DisasmState IO

type Operand = Maybe ByteString
type Instruction = (Mnemonic, Operand)

start :: Disassembler ()
start = undefined

disassemble :: ByteString -> IO DisasmState
disassemble rom = do
  let initState = DisasmState 0 [] [] rom
  (_,d) <- runStateT start initState
  return d
