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

readPRGROM :: Get InesPRGROM
readPRGROM = do
  ins <- replicateM 10000 instruction
  return $ ins

instruction :: Get (Mnemonic, Operand)
instruction = do
  (Op m mode) <- parseOpcode <$> getWord8
  operands <- case mode of
                Implied -> return Nothing
                Accumulator -> return Nothing
                Relative -> Just <$> getLazyByteString 1 -- this is signed
                Immediate -> Just <$> getLazyByteString 1
                Absolute -> Just <$> getLazyByteString 2
                Indirect -> Just <$> getLazyByteString 2
                ZeroPage -> Just <$> getLazyByteString 1
                ZeroPageIndexedX -> Just <$> getLazyByteString 1
                ZeroPageIndexedY -> Just <$> getLazyByteString 1
                AbsoluteIndexedX -> Just <$> getLazyByteString 2
                AbsoluteIndexedY -> Just <$> getLazyByteString 2
                IndirectIndexed -> Just <$> getLazyByteString 1
                IndexedIndirect -> Just <$> getLazyByteString 1
  return $ (m, operands)
