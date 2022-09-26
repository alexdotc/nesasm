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
type Operand = Maybe Word8
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
  case isJumpOrBranch m of -- placeholder
    True -> return ()
    False -> start

next :: Disassembler Opcode
next = do
  pos <- gets position
  rom <- gets prg
  updatePosition 1
  lift $ indexMaybe rom pos >>= parseOpcode

instruction :: Opcode -> Disassembler Instruction
instruction (Op m a) = do
  pos <- gets position
  rom <- gets prg
  n <- case lenOpcode a of
         0 -> return Nothing
         1 -> (return $ indexMaybe rom pos) <* updatePosition 1
         2 -> (return $ indexMaybe rom pos) <* updatePosition 2 -- TODO return 2 bytes not just 1
  return (m, n)

updatePosition :: Int -> Disassembler ()
updatePosition n = modify $ \s -> s { position = position s + n }

disassemble :: InesROM -> Maybe DisasmState
disassemble rom = do
  let initState = DisasmState 0 [] []
  d <- execStateT start (initState rom)
  return d
