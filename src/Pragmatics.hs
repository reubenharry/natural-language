{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pragmatics where


import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class
    ( MonadCond, MonadSample(uniformD, bernoulli), condition, factor )
import Control.Monad.Bayes.Enumerator ( Enumerator, mass ) 
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.Bayes.Weighted ()
import qualified Control.Monad.Bayes.Weighted as Bayes (prior)
import Control.Monad.Bayes.Traced ( mh )
import Control.Monad.State ( Monad(return) )
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
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


type Convention m =
  (IdiomGrammar m NoPausing CAT,
  IdiomInterpreter (Maybe NoPausing) (Sem M) )

type Speaker = World -> Enumerator String
type Listener = String -> Enumerator World

-- maximum tree depth we permit
cutoffDepth :: Integer
cutoffDepth = 4

-- a simple prior over worlds
prior :: Enumerator World
prior = do
  ks <- uniformD $ filter (not . null) $ filterM (const [True, False]) [John, Jane, Jill]
  let makeAttrs = (\x -> return [("runner", show x)]) =<< bernoulli 0.5
  attrs <- replicateM 3 makeAttrs
  return $ zip ks attrs

-- the convention is the syntax and semantics.
-- It's a distribution over all strings given by the grammar
-- paired with the corresponding meaning given by the semantics
convention :: MonadSample m => Convention m -> m (String, Sem M)
convention (syntax, semantics) = do
  tree <- fmap fromFree $ FT.joinFreeT $ FT.cutoff cutoffDepth $ Fold.futu syntax S
  case tree of
    Just x -> do
      string <- uniformD [Fold.cata linearize x, ""]
      let den = Fold.histo semantics x
      return (string, den)
    Nothing -> return ("", ERR "partial")


truthfulSpeaker :: Speaker
truthfulSpeaker = memo $ \w -> do
  (string, den) <- convention (syntax, semantics)
  when (string/="") $ condition (case runM w $ getSentence den of
    Right True -> True
    _ -> False)
  -- if w==w2 then trace (s÷how (w, string)) (return ()) else return ()
  pure string

listener :: Listener
listener = memo $ \u -> do
  w <- prior
  score 1 truthfulSpeaker w u
  return w


informativeSpeaker :: (MonadSample m, MonadCond m) => World -> m String
informativeSpeaker w = do
  (u, _) <- convention (syntax, semantics)
  score 100 listener u w 
  return u



w1, w2 :: World
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

exampleEnumeration :: [(String, Double)]
exampleEnumeration = normalForm2 $ informativeSpeaker w2

exampleMetropolisHastings :: IO ()
exampleMetropolisHastings = replicateM_ 10 $ do
  sample <- sampleIO $ Bayes.prior $ mh 10000 $ informativeSpeaker w2
  print (head sample)

score :: (MonadCond m, Ord a) => Double -> (t -> Enumerator a) -> t -> a -> m ()
score temperature model input output = factor (Log.Exp (temperature * log (mass (model input) output)))


-- listener' :: MonadInfer m => Listener m
-- listener' u = do
--   w <- listener "the woman runs"
--   factor (Log.Exp $ log $ mass (truthfulSpeaker w) u)
--   return w



