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
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Void ( Void, absurd ) 
import Types (BaseTree (..), CAT, NodeData (C), Word)
import Control.Monad.Bayes.Enumerator
    ( normalForm, compact, Enumerator )
import Data.Function ( ($), (.) )
import Data.Ord ( Ord(compare), Down(Down) )
import Data.List ( filter, map, sum, sortBy )
import Data.Fix ( Fix(Fix) )
import Prelude hiding (Word)


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

-- when any pause value is Nothing, return Nothing. Else convert to a simpler representation
fromFree :: FT.FreeT (BaseTree Word) Identity (Maybe Void) -> Maybe (Fix (BaseTree Word))
fromFree = Fold.cata $ \case
  (Compose (Identity (FT.Free (Leaf x)))) -> Just $ Fix $ Leaf x
  (Compose (Identity (FT.Free (Branch ls)))) -> case sequence ls of
    Nothing -> Nothing
    Just ls' -> Just $ Fix $ Branch ls'
  (Compose (Identity (FT.Pure Nothing))) -> Nothing
  (Compose (Identity (FT.Pure (Just x)))) -> absurd x

normalForm2 :: Ord a => Enumerator a -> [(a, Double)]
normalForm2 = sortBy (\x y -> compare (Down $ snd x) (Down $ snd y) ) . compact . normalize2 . filter (not . isNaN . snd) . normalForm

normalize2 :: [(a, Double)] -> [(a, Double)]
normalize2 xs = map (fmap (/ z)) xs
  where
    z = sum (fmap snd xs)

