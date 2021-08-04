{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Syntax where

import Control.Monad.Bayes.Class (MonadSample (uniformD))
import qualified Control.Monad.Free as F
import Control.Monad.Identity (Identity (Identity), Monad (return))
import Data.Functor.Compose (Compose (Compose))
import qualified Data.Functor.Foldable as Fold
import Types
  ( BaseTree (Leaf),
    CAT (..),
    Deterministic,
    IdiomGrammar,
    Grammar,
    NoPausing,
    NodeData (C),
    PauseWithCat,
  )
import Utl (branch, leaf, loadFragments, pauseAt)


syntax :: MonadSample m => IdiomGrammar m NoPausing CAT
syntax =
  Compose . \case
    S -> return $ branch (F.Pure <$> [NP, VP])
    NP -> uniformD [
      branch $ F.Pure <$> [DET, N], 
      -- branch [
      --   F.Pure NP,
      --   F.Free $ Compose $ return $ branch [F.Free $ Compose $ return $ leaf "and", F.Pure NP]], 
      leaf "John", leaf "Jane", leaf "Jill", leaf "everyone", leaf "someone"]
    DET -> uniformD [leaf "the", leaf "a"]
    N -> uniformD [
     branch $ F.Pure <$> [A, N], 
     leaf "woman", leaf "runner"]
    A -> uniformD [leaf "green", leaf "red"]
    VP ->
      uniformD
        [ leaf "runs",
          -- branch [F.Free $ Compose $ return $ leaf "is", F.Pure A],
          branch [F.Free $ Compose $ return $ leaf "is", F.Pure NP]
        ]
    TV -> return $ leaf "sees"
