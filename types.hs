module Types
  　　　　(
 　　   　Transaction(..)
  　　　, Id
  　　　, Input(..)
  　　　, Output(..)
  　　　, Address
  　　　, Index
  　　　, UTXOs
　　　　) where

import Data.Map (Map)

data Transaction =　Transaction
    { tId     :: Id
    , tInput  :: [Input]
    , tOutput :: [Output]
    } deriving Show

type Id = Int

data Output =　Output
    { oValue   :: Int
    , oAddress :: Address
    }
  deriving Show

type Address = String

data Input =　Input
    { iPrevious :: Id
    , iIndex    :: Index
    }
  deriving (Show, Eq, Ord)

type Index = Int

type UTXOs = Map Input Output
