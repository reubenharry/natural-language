{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Syntax where

import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class (MonadSample (uniformD))
import qualified Control.Monad.Free as F
import Control.Monad.Identity (Identity (Identity), Monad (return))
import qualified Control.Monad.Trans.Free as FT
import Data.Functor.Compose (Compose (Compose))
import qualified Data.Functor.Foldable as Fold
import Data.Void (Void)
import Types
  ( BaseTree (Leaf),
    CAT (..),
    Deterministic,
    FragmentDict,
    FragmentGrammar,
    Grammar,
    NoPausing,
    NodeData (C),
    PauseWithCat,
  )
import Utl (branch, leaf, loadFragments, pauseAt)
import Prelude hiding (Word)

--------------------------------------
-- progressively more complex syntaxes
--------------------------------------

syntax1 :: Grammar Deterministic NoPausing CAT
syntax2 :: Grammar Deterministic PauseWithCat CAT
syntax3 :: FragmentDict -> FragmentGrammar Deterministic NoPausing CAT
syntax4 :: FragmentDict -> FragmentGrammar Deterministic PauseWithCat CAT
syntax5 :: MonadSample m => Grammar m NoPausing CAT
syntax6 :: MonadSample m => Grammar m PauseWithCat CAT
syntax7, pureFragment :: MonadSample m => FragmentDict -> FragmentGrammar m NoPausing CAT
syntax8 :: MonadSample m => FragmentDict -> FragmentGrammar m PauseWithCat CAT
syntaxApple :: MonadSample m => FragmentGrammar m NoPausing CAT
cfg :: FragmentDict -> FragmentGrammar [] NoPausing CAT
syntax1 =
  Compose . Identity . \case
    S -> branch [NP, VP]
    NP -> branch [DET, N]
    DET -> leaf "the"
    N -> leaf "cat"
    VP -> branch [TV, NP]
    A -> leaf "red"
    TV -> leaf "sees"

syntax2 =
  Compose . Identity . \case
    S -> branch [NP, VP]
    NP -> branch [DET, N]
    DET -> leaf "the"
    N -> leaf "cat"
    VP -> pauseAt VP
    A -> leaf "red"
    TV -> leaf "sees"

syntax3 fragmentDict =
  Compose . Identity . \case
    S -> branch $ F.Pure <$> [NP, VP]
    NP -> branch $ F.Pure <$> [DET, N]
    DET -> leaf "the"
    N -> leaf "cat"
    VP -> loadFragments $ head $ fragmentDict VP
    TV -> leaf "sees"
    A -> leaf "red"

syntax4 fragmentDict =
  Compose . Identity . \case
    S -> branch (F.Pure <$> [NP, VP])
    DET -> leaf "the"
    A -> leaf "red"
    N -> leaf "cat"
    NP -> pauseAt NP
    VP -> loadFragments $ head $ fragmentDict VP
    TV -> leaf "sees"

syntax5 =
  Compose . \case
    S -> return $ branch [NP, VP]
    NP -> return $ branch [DET, N]
    DET -> return $ leaf "the"
    N -> uniformD [leaf "idea", leaf "cat"]
    A -> uniformD [leaf "green", leaf "furious"]
    VP -> return $ branch [TV, NP]
    TV -> uniformD [leaf "sees", leaf "knows"]

syntax6 =
  Compose . \case
    S -> return $ branch [NP, VP]
    NP -> return $ branch [DET, N]
    DET -> return $ leaf "the"
    N -> uniformD [branch [A, N], leaf "idea"]
    A -> uniformD [leaf "green", leaf "furious"]
    VP -> uniformD [FT.Pure VP, branch [TV, NP]]
    TV -> return $ leaf "sees"

syntax7 fragmentDict =
  Compose . \case
    S -> return $ branch (F.Pure <$> [NP, VP])
    NP -> return $ branch $ F.Pure <$> [DET, N]
    DET -> return $ leaf "the"
    N -> uniformD [branch $ F.Pure <$> [A, N], leaf "idea"]
    A -> uniformD [leaf "green", leaf "furious"]
    VP -> uniformD (loadFragments <$> fragmentDict VP)
    TV -> return $ leaf "sees"

pureFragment fragmentDict =
  Compose . \x -> uniformD (loadFragments <$> fragmentDict x)

syntax8 fragmentDict =
  Compose . \case
    S -> return $ branch (F.Pure <$> [NP, VP])
    NP -> return $ FT.Pure NP
    DET -> return $ leaf "the"
    N -> uniformD [branch $ F.Pure <$> [A, N], leaf "idea"]
    A -> uniformD [leaf "green", leaf "furious"]
    VP -> uniformD (loadFragments <$> fragmentDict VP)
    TV -> return $ leaf "sees"

syntaxApple =
  Compose . \case
    S -> return $ branch (F.Pure <$> [NP, VP])
    NP -> uniformD [
      branch $ F.Pure <$> [DET, N], 
      -- branch [
      --   F.Pure NP,
      --   F.Free $ Compose $ return $ branch [F.Free $ Compose $ return $ leaf "and", F.Pure NP]], 
      leaf "John", leaf "Jane", leaf "Jill", leaf "everyone", leaf "someone"]
    DET -> uniformD [leaf "the"]
    N -> uniformD [
     -- branch $ F.Pure <$> [A, N], 
     leaf "woman"]
    -- A -> uniformD [leaf "green", leaf "red"]
    VP ->
      uniformD
        [ leaf "runs"
          -- branch [F.Free $ Compose $ return $ leaf "is", F.Pure A]
        ]
    TV -> return $ leaf "sees"

cfg fragmentDict =
  Compose . \case
    S -> return $ branch $ F.Pure <$> [NP, VP]
    NP -> return $ branch $ F.Pure <$> [DET, N]
    DET -> return $ leaf "the"
    N -> [branch $ F.Pure <$> [A, N], leaf "idea"]
    A -> [leaf "green", leaf "furious"]
    VP -> loadFragments <$> fragmentDict VP
    TV -> return $ leaf "sees"
