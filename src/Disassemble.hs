module Disassemble (disassemble) where

import Control.Monad(replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
type Operand = Maybe [Word8]
type Instruction = (Mnemonic, Operand)

indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe b n
  | n >= B.length b = Nothing
  | otherwise = Just $ B.index b n

isJumpOrBranch :: Mnemonic -> Bool
isJumpOrBranch m = if elem m [JMP, JSR, BPL, BIT, BVC, RTS, BVS, BCC, BCS, BNE, BEQ] then True else False

start :: Disassembler ()
start = do
  opc <- next
  (m, o) <- instruction opc
  modify $ \s -> s { done = (m, o) : done s }
  case isJumpOrBranch m of -- placeholder
    True -> return ()
    False -> start

next :: Disassembler Opcode
next = do
  pos <- gets position
  rom <- gets prg
  updatePosition 1
  lift $ indexMaybe rom pos >>= parseOpcode

takeByte :: Disassembler Word8
takeByte = do
  pos <- gets position
  rom <- gets prg
  (lift $ indexMaybe rom pos) <* updatePosition 1

instruction :: Opcode -> Disassembler Instruction
instruction (Op m a) = do
  n <- case numOperands a of
         ZeroOpr -> return Nothing
         OneOpr -> return <$> replicateM 1 takeByte
         TwoOpr -> return <$> replicateM 2 takeByte
  return (m, n)

updatePosition :: Int -> Disassembler ()
updatePosition n = modify $ \s -> s { position = position s + n }

disassemble :: InesROM -> Maybe DisasmState
disassemble rom = do
  let initState = DisasmState 0 [] []
  d <- execStateT start (initState rom)
  return d
