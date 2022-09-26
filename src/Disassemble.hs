module Disassemble (disassemble) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor.Identity (Identity)
import Data.Word

import Opcode

data DisasmState = DisasmState {
                     position :: Int
                   , done :: [Instruction]
                   , stack :: [Word32]
                   , prg :: ByteString
                   } deriving Show

type Disassembler = StateT DisasmState Maybe

type InesROM = ByteString -- redundant, keep till reorg
type Operand = Maybe ByteString
type Instruction = (Mnemonic, Operand)

indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe b n
  | n >= B.length b = Nothing
  | otherwise = Just $ B.index b n

start :: Disassembler ()
start = do
  opc <- next
  (m, o) <- instruction
  return ()

next :: Disassembler Opcode
next = do
  pos <- gets position
  rom <- gets prg
  updatePosition 1
  lift $ parseOpcode <$> indexMaybe rom pos

instruction :: Disassembler Instruction
instruction = undefined

updatePosition :: Int -> Disassembler ()
updatePosition n = modify $ \s -> s { position = position s + n }

disassemble :: InesROM -> Maybe DisasmState
disassemble rom = do
  let initState = DisasmState 0 [] []
  (_,d) <- runStateT start (initState rom)
  return d
