{-# LANGUAGE RecordWildCards #-}

module Transaction
  　　　　(
  　　　　processTransactions
　　　　) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (get, modify, when, zipWithM)
import qualified Data.Map             as M
import           Data.Semigroup       ((<>))

import           App                  (App)
import           Types                (Id, Index, Input (..), Output (..),
                                       Transaction (..))

processTransaction :: Transaction -> App ()
processTransaction Transaction{..} = do
  inputValue <- processInputs tId tInput
  outputValue <- processOutputs tId tOutput
  when (inputValue < outputValue) $
    throwError $ "Insufficient amount, tId: " <> show tId
      <> "\n Input is less than output by: "
      <> show (outputValue - inputValue)

processTransactions :: [Transaction] -> App ()
processTransactions = mapM_ processTransaction

processInputs :: Id -> [Input] -> App Int
processInputs tid inputs = sum <$> mapM (processInput tid) inputs

processInput :: Id -> Input -> App Int
processInput tid input = do
  utxos <- get
  case M.lookup input utxos of
      Nothing -> throwError $ "Invalid input at: " <> show tid
      Just Output{..} -> do
        modify $ M.delete input
        return oValue

processOutputs :: Id -> [Output] -> App Int
processOutputs tid outputs = sum <$> zipWithM (processOutput tid) [0..] outputs

processOutput :: Id -> Index -> Output -> App Int
processOutput tid i output@Output{..} = do
  modify $ M.insert (Input tid i) output
  return oValue
