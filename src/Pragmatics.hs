{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pragmatics where

import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class
    ( MonadInfer, MonadSample(bernoulli, uniformD), condition, factor )
import Control.Monad.Bayes.Enumerator ( mass )
import Control.Monad.Bayes.Sampler ( sampleIO )
import Control.Monad.State ( Monad(return) )
import qualified Control.Monad.Trans.Free as FT
import qualified Data.Functor.Foldable as Fold
import Control.Monad.Identity ( Identity(runIdentity), filterM, replicateM, when )
import qualified Numeric.Log as Log
import qualified Data.Map as M

import Semantics
    ( M,
      Sem(ERR),
      World,
      Entity(John, Jill, Jane),
      semantics,
      linearize,
      getSentence,
      runM )
import Syntax ( syntax )
import Types
    ( IdiomInterpreter, IdiomGrammar, NoPausing, CAT(S) )
import Utl ( normalForm2, fromFree )
import Prelude hiding (Word, words)



type Convention m =
  (IdiomGrammar m NoPausing CAT,
  IdiomInterpreter (Maybe NoPausing) (Sem M) )

type Speaker m = World -> m String
type Listener m = String -> m World

-- maximum tree depth we permit
cutoffDepth :: Integer
cutoffDepth = 3

prior :: MonadSample m =>  m World
prior = do
  ks <- uniformD $ filter (not . null) $ filterM (const [True, False]) [John, Jane, Jill]
  let makeAttrs = (\x -> return $ M.fromList [("runner", show x)]) =<< bernoulli 0.5
  attrs <- replicateM 3 makeAttrs
  return $ M.fromList $ zip ks attrs

-- the convention is the syntax and semantics.
-- Running it generates a distribution over all strings given by the grammar
-- paired with the corresponding meaning given by the semantics
runConvention :: MonadSample m => Convention m -> m (String, Sem M)
runConvention (syntax, semantics) = do
  tree <- fmap fromFree $ FT.joinFreeT $ FT.cutoff cutoffDepth $ Fold.futu syntax S
  case tree of
    Just x -> do
      string <- uniformD [Fold.cata linearize x, ""]
      let den = Fold.histo semantics x
      return (string, den)
    Nothing -> return ("", ERR "partial")

truthfulSpeaker :: MonadInfer m => Speaker m
truthfulSpeaker w = do
  (string, den) <- runConvention (syntax, semantics)
  when (string/="") $ condition (case runM w $ getSentence den of
    Right True -> True
    _ -> False)
  pure string

listener :: MonadInfer m => Listener m
listener u = do
  w <- prior
  factor (Log.Exp $ log $ mass (truthfulSpeaker w) u)
  return w

informativeSpeaker :: MonadInfer m => Speaker m
informativeSpeaker w = do
  (string, _) <- runConvention (syntax , semantics)
  factor (Log.Exp (100 * log (mass (listener string) w )))
  return string


w1, w2 :: World
w1 = M.fromList [
  (Jane, M.fromList [
    ("runner", "True")]),
  (Jill, M.fromList [
    ("runner", "True")])]
w2 = M.fromList [
  (Jane, M.fromList [
    ("runner","True")]),
  (John, M.fromList [
    ("runner","False")])]

main :: [(String, Double)]
main = normalForm2 . informativeSpeaker $ w2




-- listener' :: MonadInfer m => Listener m
-- listener' u = do
--   w <- listener "the woman runs"
--   factor (Log.Exp $ log $ mass (truthfulSpeaker w) u)
--   return w




