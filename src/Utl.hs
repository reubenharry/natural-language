{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Utl where

import Control.Arrow ((&&&), (***))
import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import qualified Control.Monad.Free as F
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.State ()
import qualified Control.Monad.Trans.Free as FT
import Data.Functor.Compose (Compose (Compose))
import qualified Data.Functor.Foldable as Fold
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Void (Void)
import Types (BaseTree (..), CAT, NodeData (C))
import Control.Monad.Bayes.Enumerator
import Data.Function
import Data.Ord
import Data.List

branch :: [b0] -> FT.FreeF (BaseTree leafType0) a0 b0
branch = FT.Free . Branch

leaf = FT.Free . Leaf

pauseAt = FT.Pure

loadFragments brs =
  FT.Free
    ( Branch (F.hoistFree (Compose . return . FT.Free) . toFree <$> brs)
    )

toFree :: Functor f => FT.Free f a1 -> F.Free f a1
toFree = Fold.cata $ \case
  Compose (Identity (FT.Free x)) -> F.Free x
  Compose (Identity (FT.Pure x)) -> F.Pure x

normalForm2 :: Ord a => Enumerator a -> [(a, Double)]
normalForm2 = sortBy (\x y -> compare (Down $ snd x) (Down $ snd y) ) . compact . normalize2 . filter (not . isNaN . snd) . normalForm

normalize2 :: [(a, Double)] -> [(a, Double)]
normalize2 xs = map (fmap (/ z)) xs
  where
    z = sum (fmap snd xs)

