{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import qualified Control.Monad.Free as F
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
import Data.Void (Void)
import Prelude hiding (Word)
import Data.Functor.Compose ( Compose )
import Data.MemoTrie
    ( enumerateGeneric, trieGeneric, untrieGeneric, HasTrie(..), Reg )
import GHC.Generics ( Generic )
import qualified Data.Map as M
import Control.Monad.Identity ( Identity(..), filterM )
import Control.Monad.Reader
    ( MonadTrans(lift), MonadReader(ask), ReaderT(runReaderT), asks )
import Control.Monad.Cont ( ContT(..) )
import Control.Monad.Except ( MonadError(..), ExceptT, runExceptT )

------------------
-- semantics types
------------------

data Entity = Jane | John | Jill deriving (Show, Eq, Ord, Generic)
type Attrs = [(String, String)]
type World = [(Entity, Attrs)]


data Sem a
  = E (a Entity)
  | PRED1 (Entity -> a Bool)
  | BOOLEAN (a Bool)
  | QUANT ((Entity -> a Bool) -> a Entity)
  | ERR String

type M = ContT Bool (ReaderT World (ExceptT String Identity))


------------------
-- syntax types
------------------

type Word = String

data CAT = S | NP | VP | A | N | DET | TV deriving (Show, Eq, Ord)

-- mark whether a node was compositionally generated, or generated via a fragment
data NodeData = C CAT | I String

-- the core tree datatype. Importantly, it's not a recursive type
data BaseTree leafType branchType
  = Leaf leafType
  | Branch [branchType]
  deriving (Functor, Foldable, Traversable)

-- some simple name synonyms to make types built with these types more self-explanatory to read
type Deterministic = Identity -- a type synonym to indicate that the monad is just the identity

type NoPausing = Void
type PauseWithCat = CAT

type RecursivelyBranch m pauseType = FT.FreeT (BaseTree Word) m pauseType
type Fragment m = RecursivelyBranch m PauseWithCat

-- The Base type family maps RecursivelyBranch to this
type NonRecursivelyBranch m pauseType = Compose m (FT.FreeF (BaseTree Word) pauseType)




type Grammar m pauseType startType = startType -> NonRecursivelyBranch m pauseType startType
type Interpreter resultType = BaseTree Word resultType -> resultType

type IdiomGrammar m pauseType startType =
  startType ->
    (NonRecursivelyBranch m pauseType)
    (F.Free (NonRecursivelyBranch m pauseType) startType)


type IdiomInterpreter pauseType resultType =
  (BaseTree Word)
    (F.Cofree (BaseTree Word) resultType) ->
  resultType




-- generic instance for memoization in Pragmatics.hs
instance HasTrie Entity where
  newtype (Entity :->: b) = EntityTrie { unEntityTrie :: Reg Entity :->: b } 
  trie = trieGeneric EntityTrie 
  untrie = untrieGeneric unEntityTrie
  enumerate = enumerateGeneric unEntityTrie
