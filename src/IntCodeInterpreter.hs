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
  , PureCPU(..)
  , freezeCPU
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

data PureCPU = PureCPU
  { pureMemory       :: Vector.Vector Int
  , pureIP           :: Address
  , pureRelativeBase :: Address
  } deriving (Show, Eq)

data CPU m = CPU
  { memory       :: (MutVar (PrimState m) (Memory m))
  , ip           :: (MutVar (PrimState m) Address)
  , relativeBase :: (MutVar (PrimState m) Address)
  }

data CPUState m r a where
  GetIP :: CPUState m r Address
  SetIP :: Address -> CPUState m r ()
  ReadAddr :: Address -> CPUState m r Int
  WriteAddr :: Address -> Int -> CPUState m r ()
  AddRelativeBase :: Address -> CPUState m r ()
  GetRelativeBase :: CPUState m r Address
  TraceCPUState :: CPUState m r String
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
    mem <- readMemory memory (Address addr)
    MVector.read mem addr
  WriteAddr (Address addr) value -> embed $ do
    mem <- readMemory memory (Address addr)
    MVector.write mem addr value
  AddRelativeBase addr -> embed $
    modifyMutVar relativeBase (+addr)
  GetRelativeBase -> embed $
    readMutVar relativeBase
  TraceCPUState -> embed $ do
    mem <- Vector.freeze =<< readMutVar memory
    relativeBase' <- readMutVar relativeBase
    ip' <- readMutVar ip
    pure $ show mem <> " RB: " <> show relativeBase' <> " IP: " <> show ip'


readMemory :: PrimMonad m => MutVar (PrimState m) (Memory m) -> Address -> m (Memory m)
readMemory memory' (Address addr) = do
  mem <- readMutVar memory'
  mem' <- if (addr < MVector.length mem)
    then pure mem
    else do
      let len = MVector.length mem
      v <- MVector.grow mem $ max (addr + 1) (MVector.length mem * 2)
      forM_ [len..len*2-1] $ \i -> MVector.write v i 0
      pure v

  writeMutVar memory' mem'
  pure mem'

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
readValueAt i =
  readAddrAt i >>= readAddr

readAddrAt
  :: Members '[CPUState m, State Mode] r
  => PrimMonad m
  => Address
  -> Sem r Address
readAddrAt i = do
  (mode', mode) <- gets @Mode (`divMod` 10)
  addr <- case mode of
        0 -> Address <$> readAddr i
        1 -> pure i
        2 -> do
          rb <- getRelativeBase
          addr <- Address <$> readAddr i
          pure $ rb + addr
        _ -> error "unknown parameter mode"
  put mode'
  pure addr


applyBinaryOperation
  :: Members '[CPUState m, State Mode] r
  => PrimMonad m
  => (Int -> Int -> Int)
  -> Sem r ()
applyBinaryOperation f = do
  pos <- getIP
  i <- readValueAt (pos+1)
  j <- readValueAt (pos+2)
  storeAt <- readAddrAt (pos+3)
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
  storeAt <- readAddrAt (pos+3)
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
  -- cpuStateStr <- traceCPUState
  -- modeAndOperation <- traceShow cpuStateStr $ readArg 0
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
    (mode, 3) -> input >>= \case
      Nothing ->
        pure ()
      Just (InterpreterInput i) -> do
        storeAt <- evalState mode $ readAddrAt (pos+1)
        writeAddr storeAt i
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
    (mode, 9) -> do
      adj <- evalState mode (readValueAt (pos+1))
      addRelativeBase (Address adj)
      addIP 2
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
  relativeBase <- newMutVar 0
  pure CPU{..}

freezeCPU
  :: PrimMonad m
  => CPU m
  -> m (PureCPU)
freezeCPU CPU{..} = do
  pureMemory <- Vector.freeze =<< readMutVar memory
  pureIP <- readMutVar ip
  pureRelativeBase <- readMutVar relativeBase
  pure PureCPU{..}

createCPUfromCode
  :: PrimMonad m
  => PureCPU
  -> m (CPU m)
createCPUfromCode PureCPU{..} = do
  memory <- newMutVar =<< Vector.thaw pureMemory
  ip <- newMutVar pureIP
  relativeBase <- newMutVar pureRelativeBase
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
eval' intCode = snd . eval intCode

eval
  :: [Int]
  -> [InterpreterInput]
  -> (PureCPU, [InterpreterOutput])
eval intCode is = runST $ do
    cpu@CPU{..} <- createCPU intCode
    out <- evalM cpu is
    pureCPU <- freezeCPU cpu
    pure (pureCPU, out)

evalM
  :: PrimMonad m
  => CPU m
  -> [InterpreterInput]
  -> m [InterpreterOutput]
evalM cpu is = interpretEvalM is cpu step
