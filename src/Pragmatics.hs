{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Pragmatics where


import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class hiding (score)
import Control.Monad.Bayes.Enumerator ( Enumerator, enumerate, mass )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Bayes.Weighted hiding (prior)
import qualified Control.Monad.Bayes.Weighted as Bayes (prior)
import Control.Monad.Bayes.Traced ( mh )
import Control.Monad.State ( Monad(return) )
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
import Control.Monad.Reader
import Control.Monad.Identity
    ( Monad(return),
      Functor(fmap),
      filterM,
      replicateM,
      replicateM_,
      (=<<),
      when )
import qualified Numeric.Log as Log
import qualified Data.Map as M
import Semantics ( semantics, linearize, getSentence, runM )
import Syntax ( syntax )
import Types
    ( CAT(S),
      IdiomInterpreter,
      IdiomGrammar,
      NoPausing,
      M,
      Sem(ERR),
      World,
      Entity(Jane, Jill, John) )
import Utl ( normalForm2, fromFree )
import Prelude hiding (Word, words)
import Data.MemoTrie ( memo )
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import System.IO
import System.Random
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)


type Convention m =
  (IdiomGrammar m NoPausing CAT, IdiomInterpreter (Maybe NoPausing) (Sem M) ) -> m (String, Sem M)

-- type Speaker = World -> Enumerator String
-- type Listener = String -> Enumerator World
type Prior m = m World
-- maximum tree depth we permit
cutoffDepth :: Integer
cutoffDepth = 3

-- a simple prior over worlds
-- prior :: MonadSample m => m World
prior :: Enumerator World
prior = do
  ks <- uniformD $ filter (not . null) $ filterM (const [True, False]) [John, Jane, Jill]
  let makeAttrs = (\x -> return [("runner", show x)]) =<< bernoulli 0.5
  attrs <- replicateM 3 makeAttrs
  return $ zip ks attrs

-- the convention is the syntax and semantics.
-- It's a distribution over all strings given by the grammar
-- paired with the corresponding meaning given by the semantics
convention :: MonadSample m => Convention m
-- convention :: Convention Enumerator -> Enumerator (String, Sem M)
convention (syntax, semantics) = do
  tree <- fmap fromFree $ FT.joinFreeT $ FT.cutoff cutoffDepth $ Fold.futu syntax S
  case tree of
    Just x -> do
      string <- uniformD [Fold.cata linearize x, ""]
      let den = Fold.histo semantics x
      return (string, den)
    Nothing -> return ("", ERR "partial")


-- truthfulSpeaker :: MonadInfer m => World -> m String
truthfulSpeaker :: World -> Enumerator String
truthfulSpeaker = memo $ \w -> do
  (string, den) <- convention (syntax, semantics)
  when (string/="") $ condition (case runM w $ getSentence den of
    Right True -> True
    _ -> False)
  -- if w==w2 then trace (s÷how (w, string)) (return ()) else return ()
  pure string

listener :: MonadInfer m => String -> m World -> m World
-- listener :: String -> Prior Enumerator -> Enumerator World
listener = memo $ \u p -> do
  w <- p
  score 1 (truthfulSpeaker w) u
  return w

informativeSpeaker :: MonadInfer m => World -> Prior Enumerator -> m String
-- informativeSpeaker :: World -> Prior Enumerator -> Enumerator String
informativeSpeaker w prior = do
  (u, _) <- convention (syntax, semantics)
  let l = listener u prior
  score 100 l w
  return u



w1, w2, w3, w4 :: World
w1 = [
  (Jane, [
    ("runner", "True")]),
  (Jill, [
    ("runner", "True")])]
w2 = [
  (John, [
    ("runner","False")]),
  (Jane, [
    ("runner","True")])
  ]
w3 = [
  (John, [
    ("runner","True")]),
  (Jane, [
    ("runner","True")]),
  (Jill, [
    ("runner","True")])
  ]
w4 = [
  (Jane, [
    ("runner","False")])]

exampleEnumeration :: [(String, Double)]
exampleEnumeration = normalForm2 (informativeSpeaker w2 prior)

-- exampleMetropolisHastings :: IO ()
-- exampleMetropolisHastings = replicateM_ 10 $ do
--   sample <- sampleIO $ Bayes.prior $ mh 10000 $ (informativeSpeaker w2) prior
--   print (head sample)

score :: (MonadCond m, Ord a) => Double -> Enumerator a -> a -> m ()
score temperature model output = factor (Log.Exp (temperature * log (mass model output)))



