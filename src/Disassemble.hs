module Disassemble (disassemble) where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Array.Unboxed (UArray, (//), (!))
import qualified Data.Array.Unboxed as UA
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int8)
import Data.Word

import Opcode

data DisasmState = DisasmState {
                     position :: Int
                   , assembly :: [Instruction]
                   , stack :: [Word16]
                   , done :: UArray Int Bool
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

isBranch :: Mnemonic -> Bool
isBranch m = if elem m [BPL, BMI, BVC, BNE, BEQ, BCC, BCS, BVS] then True else False

start :: Disassembler ()
start = do
  checkDone
  opc <- opcode
  (m, o) <- instruction opc
  modify $ \s -> s { assembly = (m, o) : assembly s }
  case isBranch m of
    True -> branch o >> start
    False -> case m of
      JMP -> return () -- handle 2 types of jmp
      RTI -> return () -- placeholder
      JSR -> pushSubroutine o
      RTS -> popSubroutine
      _   -> start

checkDone :: Disassembler ()
checkDone = do
  pos <- gets position
  d <- gets done
  case (d ! pos) of
    True -> updatePosition 1 >> checkDone
    False -> return ()

branch :: Operand -> Disassembler ()
branch (Just (o:_)) = updatePosition $ fromIntegral (fromIntegral o :: Int8) -- signed 8 bit value

fuseAbsoluteOperand :: Operand -> Word16
fuseAbsoluteOperand (Just (lsb:msb:[])) = (shiftL (fromIntegral msb :: Word16) 8) .|. (fromIntegral lsb :: Word16)

pushSubroutine :: Operand -> Disassembler ()
pushSubroutine o = do
  let t = fuseAbsoluteOperand o
  modify $ \s -> s { stack = (fromIntegral (position s) :: Word16):(stack s) 
                              , position = fromIntegral $ t - 0x8000 } -- maybe? TODO

popSubroutine :: Disassembler ()
popSubroutine = do
  (top:rest) <- gets stack
  modify $ \s -> s { stack = rest, position = fromIntegral top }
  

opcode :: Disassembler Opcode
opcode = do
  pos <- gets position
  rom <- gets prg
  markDone
  updatePosition 1
  lift $ indexMaybe rom pos >>= parseOpcode

takeByte :: Disassembler Word8
takeByte = do
  pos <- gets position
  rom <- gets prg
  (lift $ indexMaybe rom pos) <* markDone <* updatePosition 1

instruction :: Opcode -> Disassembler Instruction
instruction (Op m a) = do
  n <- case numOperands a of
         ZeroOpr -> return Nothing
         OneOpr -> return <$> replicateM 1 takeByte
         TwoOpr -> return <$> replicateM 2 takeByte
  return (m, n)

markDone :: Disassembler ()
markDone = modify $ \s -> s { done = done s // [(position s, True)] }

updatePosition :: Int -> Disassembler ()
updatePosition n = modify $ \s -> s { position = position s + n }

disassemble :: InesROM -> Maybe DisasmState
disassemble rom = do
  let initState = DisasmState 0 [] [] $ UA.array (0, B.length rom - 1) [(x,False) | x <- [0..B.length rom - 1]]
  d <- execStateT start (initState rom)
  return d
