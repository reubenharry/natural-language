{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pragmatics where

import Control.Arrow ((&&&), (***))
import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Bayes.Traced ( mh )
import Control.Monad.Bayes.Weighted ( prior )
import qualified Control.Monad.Free as F
import Control.Monad.State ( Monad(return) )
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
import Data.List (intersperse, sortOn, sortBy)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Void (Void)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.Except
import qualified Numeric.Log as Log
import Data.Ord
import qualified Data.Map as M

import Semantics 
import Syntax
import Types 
import Utl 
import Prelude hiding (Word, words)
import Data.Either (fromRight)
import Control.Monad.Bayes.Population (normalize)
import Data.Function
import Data.Monoid

-- import Data.Functor.Compose
-- import Data.Bifunctor
-- import Data.Functor.Const
-- import Data.Bitraversable

refold :: Monad m => 
  FragmentInterpreter Identity (Maybe NoPausing ) (Sem M) -> 
  FragmentGrammar m NoPausing CAT -> CAT -> 
  m (Sem M)
refold a b cat = fmap (runIdentity . Fold.histo a) (t b cat)
  
cutoffDepth = 5

t b cat = FT.joinFreeT $ FT.cutoff cutoffDepth $ Fold.futu b cat

type Speaker m = World -> m String
type Listener m = String -> m World

-- runM :: World -> M a -> Either String Bool
runM w x = runIdentity $ runExceptT $ flip runReaderT w $ runContT (fromRight (return False) x) return

truthfulSpeaker :: MonadInfer m => Speaker m
truthfulSpeaker w = do
  tree <- t syntaxApple S
  let den = runIdentity . Fold.histo semanticsApple $ tree 
  let string = runIdentity . Fold.histo linearize $ tree
  let b = runM w $ getSentence den
  escape <- bernoulli 0.01
  s <- if escape then pure "nil" else condition (case b of
    Right True -> True
    _ -> False) >> pure string
  return s

truthfulSpeaker' w = do
  tree <- t syntaxApple S
  let den = runIdentity . Fold.histo semanticsApple $ tree 
  let string = runIdentity . Fold.histo linearize $ tree
  let b = runM w $ getSentence den
  return (string, b)

ignoringListener :: MonadSample m => Listener m
ignoringListener _ = do 
  ks <- uniformD $ filter (not . null) $ filterM (const [True, False]) [John, Jane, Jill]
  let makeAttrs = (\x -> return $ M.fromList [("runner", show x)]) =<< bernoulli 0.5
  attrs <- replicateM 3 makeAttrs 
  return $ M.fromList $ zip ks attrs


listener :: MonadInfer m => Listener m
listener u = do
  w <- ignoringListener u
  factor (Log.Exp $ log $ mass (truthfulSpeaker w) u)
  return w

listener' :: MonadInfer m => Listener m
listener' u = do
  w <- listener "the woman runs"
  factor (Log.Exp $ log $ mass (truthfulSpeaker w) u)
  return w

-- informativeSpeaker :: MonadInfer m => Speaker m
informativeSpeaker w = do
  -- string <- uniformD ["Jane runs"] -- 
  tree <- t syntaxApple S
  string <- uniformD [runIdentity . Fold.histo linearize $ tree, "nil"]
  factor (Log.Exp (log $ mass (listener string) w ))
  return string
  -- return $ mass2 (listener string) w

w1 = M.fromList [(Jane, M.fromList [("runner", "True")]), (Jill, mempty)]

w2 = M.fromList [(Jane, M.fromList [("runner","True")]),(John, M.fromList [("runner","False")])]
-- fromList [(Jane,fromList [("runner","True")]),(John,fromList [("runner","False")])]
-- main = normalForm2 . truthfulSpeaker
-- main = normalForm2 . ignoringListener
main = normalForm2 . informativeSpeaker
-- main = normalForm2 . listener
-- main = normalForm2 . truthfulSpeaker'

-- main :: IO ()
-- main = mass (listener "John runs") [Jill]

-- -- convention = (semanticsAndPhonology1, syntax5)


-- | Normalized probability mass of a specific value.
mass2 :: Ord a => Enumerator a -> a -> Double
mass2 d = f
  where
    f a = fromMaybe 0 $ lookup a m
    m = normalForm2 d



-- refold a b = fmap (fmap (runIdentity . Fold.histo a)) . unf b

-- unf :: Monad m => FragmentGrammar m NoPausing CAT -> CAT -> m (Maybe (FT.FreeT (BaseTree Word) Identity (NoPausing)))
-- unf b = fmap (Fold.transverse (unMaybe)) . FT.joinFreeT . FT.cutoff 4 . Fold.futu b

-- unMaybe :: NonRecursivelyBranch Identity (Maybe NoPausing) (Maybe a)
--   -> Maybe (NonRecursivelyBranch Identity NoPausing a)
-- unMaybe (Compose (Identity x)) = Compose . Identity <$> (
--   case x of 
--     FT.Free k -> Just $ FT.Free (fromMaybe undefined <$> k)
--     FT.Pure _ -> Nothing)
-- -- unMaybe :: Monad m => NonRecursivelyBranch m (Maybe NoPausing) (Maybe a) -> Maybe (NonRecursivelyBranch m Void a)
-- -- unMaybe (Compose x) = undefined -- fmap undefined x -- FT.FreeT (((\x -> bimap (const $ Const Nothing) (fmap (const $ Const Nothing)) x)) <$> x) -- Compose . Identity . FT.Pure <$> x










