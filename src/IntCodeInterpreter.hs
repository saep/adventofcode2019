{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module IntCodeInterpreter
  ( parseIntCodes
  , eval
  , eval'
  , evalM
  , Code
  , InterpreterInput(..)
  , InterpreterOutput(..)
  , CPU
  , Address
  , createCPU
  , createCPUfromCode
  , showCPU
  ) where

import PreludeAoC
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Polysemy.Input
import Polysemy.Output
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Primitive.MutVar

parseIntCodes :: Parser [Int]
parseIntCodes = L.signed space L.decimal `sepBy` char ','

newtype InterpreterInput = InterpreterInput Int
  deriving (Eq, Ord, Show)
  deriving Num via Int

newtype InterpreterOutput = InterpreterOutput Int
  deriving (Eq, Ord, Show)
  deriving Num via Int

type Code = Vector.Vector Int

type Memory m = MVector.MVector (PrimState m) Int

newtype Address = Address { getAddress :: Int }
  deriving (Eq, Ord, Show)
  deriving Num via Int


data CPU m = CPU
  { memory :: (MutVar (PrimState m) (Memory m))
  , ip     :: (MutVar (PrimState m) Address)
  }

data CPUState m r a where
  GetIP :: CPUState m r Address
  SetIP :: Address -> CPUState m r ()
  ReadAddr :: Address -> CPUState m r Int
  WriteAddr :: Address -> Int -> CPUState m r ()
makeSem ''CPUState

runCPUState
  :: PrimMonad m
  => Member (Embed m) r
  => CPU m
  -> Sem (CPUState m  ': r) a
  -> Sem r a
runCPUState CPU{..} = interpret $ \case
  GetIP -> embed $
    readMutVar ip
  SetIP addr -> embed $
    writeMutVar ip addr
  ReadAddr (Address addr) -> embed $ do
    mem <- readMutVar memory
    MVector.unsafeRead mem addr
  WriteAddr (Address addr) value -> embed $ do
    mem <- readMutVar memory
    MVector.unsafeWrite mem addr value

addIP
  :: Members '[CPUState m] r
  => PrimMonad m
  => Address
  -> Sem r ()
addIP i = 
  setIP . (+i) =<< getIP


readArg
  :: Members '[CPUState m] r
  => Int
  -> Sem r Int
readArg i =
  readAddr . (+ (Address i)) =<< getIP

type Mode = Int

readValueAt
  :: Members '[CPUState m, State Mode] r
  => PrimMonad m
  => Address
  -> Sem r Int
readValueAt i = do
  (mode', mode) <- gets @Mode (`divMod` 10)
  posToRead <- case mode of
        0 -> Address <$> readAddr i
        1 -> pure i
        _ -> error "unknown parameter mode"
  put mode'
  readAddr posToRead

applyBinaryOperation
  :: Members '[CPUState m, State Mode] r
  => PrimMonad m
  => (Int -> Int -> Int)
  -> Sem r ()
applyBinaryOperation f = do
  pos <- getIP
  i <- readValueAt (pos+1)
  j <- readValueAt (pos+2)
  storeAt <- Address <$> readArg 3
  writeAddr storeAt (f i j)
  addIP 4

testPredicate
  :: Members '[CPUState m, State Mode, Embed m] r
  => PrimMonad m
  => (Int -> Int -> Bool)
  -> Sem r ()
testPredicate p = do
  pos <- getIP
  i <- readValueAt (pos+1)
  j <- readValueAt (pos+2)
  storeAt <- Address <$> readAddr (pos+3)
  addIP 4
  if p i j
    then writeAddr storeAt 1
    else writeAddr storeAt 0

jumpIf
  :: Members '[CPUState m, State Mode] r
  => PrimMonad m
  => (Int -> Bool)
  -> Sem r ()
jumpIf p = do
  pos <- getIP
  i <- readValueAt (pos+1)
  if (p i)
    then setIP . Address =<< readValueAt (pos+2)
    else addIP 3

step
  :: Members '[CPUState m, Input (Maybe InterpreterInput), Output InterpreterOutput, Embed m] r
  => PrimMonad m
  => Sem r ()
step = do
  modeAndOperation <- readArg 0
  pos <- getIP
  case modeAndOperation `divMod` 100 of
    (_, 99) ->
      pure ()
    (mode, 1) -> do
      evalState mode (applyBinaryOperation (+))
      step
    (mode, 2) -> do
      evalState mode (applyBinaryOperation (*))
      step
    (_, 3) -> input >>= \case
      Nothing ->
        pure ()
      Just (InterpreterInput i) -> do
        storeAt <- readArg 1
        writeAddr (Address storeAt) i
        addIP 2
        step
    (mode, 4) -> do
      output . InterpreterOutput =<< evalState mode (readValueAt (pos+1))
      addIP 2
      step
    (mode, 5) -> do
      evalState mode (jumpIf (/= 0))
      step
    (mode, 6) -> do
      evalState mode (jumpIf (== 0))
      step
    (mode, 7) -> do
      evalState mode (testPredicate (<))
      step
    (mode, 8) -> do
      evalState mode (testPredicate (==))
      step
    (_, op) ->
      error $ "unknown opcode: " <> show op

interpretEvalM
  :: PrimMonad m
  => [InterpreterInput]
  -> CPU m
  -> Sem '[CPUState m, Input (Maybe InterpreterInput), Output InterpreterOutput, Embed m] ()
  -> m [InterpreterOutput]
interpretEvalM is cpu =
  fmap fst . runM . runOutputList . runInputList is . runCPUState cpu

createCPU
  :: PrimMonad m
  => [Int]
  -> m (CPU m)
createCPU intCode = do
  code <- Vector.unsafeThaw $ Vector.fromList intCode
  memory <- newMutVar code
  ip <- newMutVar 0
  pure CPU{..}

createCPUfromCode
  :: PrimMonad m
  => Code
  -> Address
  -> m (CPU m)
createCPUfromCode code pos = do
  memory <- newMutVar =<< Vector.thaw code
  ip <- newMutVar pos
  pure CPU{..}

showCPU :: PrimMonad m => CPU m -> m String
showCPU CPU{..} = do
  mem <- readMutVar memory
  cpu <- Vector.freeze mem
  instrP <- readMutVar ip
  pure $ "CPU: " <> show cpu <> " IP: " <> show instrP

eval'
  :: [Int]
  -> [InterpreterInput]
  -> [InterpreterOutput]
eval' intCode = (\(_,_,out) -> out) . eval intCode

eval
  :: [Int]
  -> [InterpreterInput]
  -> (Code, Address, [InterpreterOutput])
eval intCode is = runST $ do
    cpu@CPU{..} <- createCPU intCode
    out <- evalM cpu is
    code <- Vector.unsafeFreeze =<< readMutVar memory
    ip' <- readMutVar ip
    pure (code, ip', out)

evalM
  :: PrimMonad m
  => CPU m
  -> [InterpreterInput]
  -> m [InterpreterOutput]
evalM cpu is = interpretEvalM is cpu step
