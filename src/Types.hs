{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Types where

import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import qualified Control.Monad.Free as F
import Control.Monad.Identity ( Functor, Identity )
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
import Data.Void (Void)
import Prelude hiding (Word)
import Data.Functor.Compose
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
type Tree m = RecursivelyBranch m NoPausing
type Idiom = [Fragment Deterministic]
type FragmentDict = CAT -> [Idiom]

-- The Base type family maps RecursivelyBranch to this
type NonRecursivelyBranch m pauseType = Compose m (FT.FreeF (BaseTree Word) pauseType)




type Grammar m pauseType startType = startType -> NonRecursivelyBranch m pauseType startType

type Interpreter m pauseType resultType = NonRecursivelyBranch m pauseType (m resultType) -> m resultType

type FragmentGrammar m pauseType startType =
  startType ->
    (NonRecursivelyBranch m pauseType)
    (F.Free (NonRecursivelyBranch m pauseType) startType)


type FragmentInterpreter m pauseType resultType =
  (NonRecursivelyBranch m pauseType)
    (F.Cofree (NonRecursivelyBranch m pauseType) (m resultType)) ->
  m resultType
