{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE DerivingVia #-}
module IntCodeInterpreter
  ( parseIntCodes
  , eval
  , eval'
  , InterpreterInput(..)
  , InterpreterOutput(..)
  , Code
  ) where

import PreludeAoC
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Polysemy.Input
import Polysemy.Output
import qualified Text.Megaparsec.Char.Lexer as L

parseIntCodes :: Parser [Int]
parseIntCodes = L.signed space L.decimal `sepBy` char ','

newtype InterpreterInput = InterpreterInput Int
  deriving (Eq, Ord, Show)
  deriving Num via Int

newtype InterpreterOutput = InterpreterOutput Int
  deriving (Eq, Ord, Show)
  deriving Num via Int

type Code = Vector.Vector Int

type Mode = Int

readValueAt
  :: Member (State Mode) r
  => Int
  -> Code
  -> Sem r Int
readValueAt i code = do
  (mode', mode) <- gets @Mode (`divMod` 10)
  let posToRead = case mode of
        0 -> code Vector.! i
        1 -> i
        _ -> error "unknown parameter mode"
  put mode'
  pure $ code Vector.! posToRead

applyBinaryOperation
  :: Member (State Mode) r
  => (Int -> Int -> Int)
  -> Int
  -> Code
  -> Sem r Code
applyBinaryOperation f pos code = do
  i <- readValueAt (pos+1) code
  j <- readValueAt (pos+2) code
  let store = code Vector.! (pos+3)
  pure $ runST $ do
    mem <- Vector.unsafeThaw code
    MVector.unsafeWrite mem store (f i j)
    Vector.unsafeFreeze mem

testPredicate
  :: Member (State Mode) r
  => (Int -> Int -> Bool)
  -> Int
  -> Code
  -> Sem r Code
testPredicate p pos code = do
  i <- readValueAt (pos+1) code
  j <- readValueAt (pos+2) code
  let store = code Vector.! (pos+3)
  pure $ runST $ do
    mem <- Vector.unsafeThaw code
    if p i j
      then MVector.unsafeWrite mem store 1
      else MVector.unsafeWrite mem store 0
    Vector.unsafeFreeze mem

jumpIf
  :: Member (State Mode) r
  => (Int -> Bool)
  -> Int
  -> Code
  -> Sem r Int
jumpIf p pos code = do
  i <- readValueAt (pos+1) code
  if (p i)
    then readValueAt (pos+2) code
    else pure (pos+3)

step
  :: Members '[Input (Maybe InterpreterInput), Output InterpreterOutput] r
  => Int
  -> Code
  -> Sem r Code
step pos code = do
  case (code Vector.! pos) `divMod` 100 of
    (_, 99) ->
      pure code
    (mode, 1) ->
      step (pos+4) =<< evalState mode (applyBinaryOperation (+) pos code)
    (mode, 2) ->
      step (pos+4) =<< evalState mode (applyBinaryOperation (*) pos code)
    (_, 3) -> input >>= \case
      Nothing ->
        error "No more input"
      Just (InterpreterInput i) ->
        step (pos+2) $ runST $ do
          mem <- Vector.unsafeThaw code
          storeAt <- MVector.unsafeRead mem (pos+1)
          MVector.unsafeWrite mem storeAt i
          Vector.unsafeFreeze mem
    (mode, 4) -> do
      output . InterpreterOutput =<< evalState mode (readValueAt (pos+1) code)
      step (pos+2) code
    (mode, 5) -> do
      flip step code =<< evalState mode (jumpIf (/= 0) pos code)
    (mode, 6) -> do
      flip step code =<< evalState mode (jumpIf (== 0) pos code)
    (mode, 7) -> do
      step (pos+4) =<< evalState mode (testPredicate (<) pos code)
    (mode, 8) ->
      step (pos+4) =<< evalState mode (testPredicate (==) pos code)

    (_, op) ->
      error $ "unknown opcode: " <> show op

interpretEval
  :: [InterpreterInput]
  -> Sem '[Input (Maybe InterpreterInput), Output InterpreterOutput] a
  -> ([InterpreterOutput], a)
interpretEval is =
  run . runOutputList . runInputList is


eval
  :: [InterpreterInput]
  -> [Int]
  -> ([InterpreterOutput], Vector.Vector Int)
eval is code = interpretEval is $ step 0 (Vector.fromList code)

eval'
  :: [InterpreterInput]
  -> [Int]
  -> [InterpreterOutput]
eval' is = fst . eval is
