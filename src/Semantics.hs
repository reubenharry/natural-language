{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantics where

import Control.Applicative ( Applicative(liftA2, pure) )
import Control.Comonad.Cofree ( Cofree((:<)) )
import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.State (MonadState (get))
import Data.Functor.Compose (Compose (Compose))
import Types (BaseTree (Branch, Leaf), CAT, IdiomInterpreter, Grammar, Interpreter)
import Prelude hiding (Word, words)
import Control.Monad.Identity ( Identity(..), filterM )
import Control.Monad.Reader
    ( MonadTrans(lift), MonadReader(ask), ReaderT(runReaderT), asks )
import Control.Monad.Cont ( ContT(..) )
import Control.Monad.Except ( MonadError(..), ExceptT, runExceptT )
import qualified Data.Map as M
import Data.Either (fromRight)
import Data.Fix ( Fix(Fix) )

import Data.Functor.Foldable (histo)

data Entity = Jane | John | Jill deriving (Show, Eq, Ord)
type Attrs = M.Map String String
type World = M.Map Entity Attrs

data Sem a
  = E (a Entity)
  | PRED1 (Entity -> a Bool)
  | Boolean (a Bool)
  | DET ((Entity -> a Bool) -> a Entity)
  | ERR String

-- an example syntax tree
exampleTree :: Fix (BaseTree String)
exampleTree = Fix $ Branch [
  Fix $ Branch [
    Fix $ Leaf "the",
    Fix $ Leaf "woman"],
  Fix $ Branch [
    Fix $ Leaf "is",
    Fix $ Branch [
      Fix $ Leaf "a",
      Fix $ Leaf "runner"]
  ]]

exampleInterpretation :: Either String Bool
exampleInterpretation =
  let unpack = runM exampleWorld . getSentence 
      meaning = histo semantics 
  in unpack $ meaning exampleTree

exampleWorld :: M.Map Entity (M.Map [Char] [Char])
exampleWorld = M.fromList [
  (Jane, M.fromList [
    ("runner", "True")]),
  (Jill, M.fromList [
    ("runner", "True")])]

semantics :: IdiomInterpreter pauseType (Sem M)
semantics = \case

  Word "John" -> E $ pure John
  Word "Jane" -> E $ pure Jane
  Word "Jill" -> E $ pure Jill
  Word "runner" -> PRED1 run
  Word "runs" -> PRED1 run


  Word "woman" -> PRED1 $ pure . \case
    Jane -> True
    Jill -> True
    John -> False

  Word "the" -> DET $ \x -> do 
    w <- asks M.keys
    y <- filterM x w
    case y of
      [uniqueEntity] -> return uniqueEntity
      xs -> throwError ("presup failure: " <> show xs)

  Word "everyone" -> E $ do 
    dom <- asks M.keys
    ContT $ \f -> foldr1 (liftA2 (&&)) $ fmap f dom

  Word "someone" -> E $ do
    dom <- asks M.keys 
    ContT $ \f -> foldr1 (liftA2 (||)) $ fmap f dom

  Word x -> E $ throwError $ show x

  Branch [
    _ :< Word "runs",
    _ :< Word "wild"
    ] -> PRED1 $ \x -> do
      w <- ask
      attrs <- case M.lookup x w of 
        Nothing -> throwError "Nothing runs"
        Just y -> return y
      return $ M.lookup "runner" attrs == Just "True" 

  -- is an X
  Branch [
      _ :< Word "is",
      _ :< Branch [
          _ :< Word "a", 
          PRED1 p :< _]]
    -> PRED1 p


  -- a and b
  Branch 
    [(E e1) :< _,
      _ :<  (Branch 
        [_ :< ( ( (Word "and"))),
        ( (E e2)) :< _])]
    -> E $ do
      e1' <- e1
      e2' <- e2
      ContT $ \f -> liftA2 (&&) (f e1') (f e2')

  Composition (E str) (PRED1 vp) -> Boolean $ vp =<< str
  Composition (DET det) (PRED1 n) -> E $ det n
  Composition (ERR a) (ERR b) -> ERR (a <> " " <> b)
  Composition (ERR a) _ -> ERR a
  Composition _ (ERR b) -> ERR b

  _ -> ERR "no interpretation for this"        


linearize :: Interpreter String
linearize = \case

  Leaf x -> x
  Branch [r1, r2]  -> r1 <> " " <> r2
  _ -> "Cannot interpret partial trees"

pattern Composition r1 r2 <- (Branch [r1 :< _, r2 :< _])

pattern Word :: leafType -> BaseTree leafType branchType
pattern Word str <- Leaf str

--------------------
-- word meaning
--------------------
run :: (MonadReader (M.Map k (M.Map [Char] [Char])) m, Ord k, MonadError [Char] m) => k -> m Bool
run x = do

  w <- ask
  attrs <- case M.lookup x w of 
    Nothing -> throwError "Nothing runs"
    Just y -> return y
  return $ M.lookup "runner" attrs == Just "True"


--------------------
-- utls
--------------------

getSentence :: Sem a -> Either String (a Bool)
getSentence (Boolean b) = Right b
getSentence (ERR s) = Left s
getSentence _ = Left "error"

type M = ContT Bool (ReaderT World (ExceptT String Identity))

runM :: r -> Either a (ContT Bool (ReaderT r (ExceptT e Identity)) Bool) -> Either e Bool
runM w x = runIdentity $ runExceptT $ flip runReaderT w $ runContT (fromRight (return False) x) return


-- an instance to ensure that adding ContT to the stack preserves the ability to throw errors
instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

instance Show (Sem a) where
  show (E _) = "E"
  show (PRED1 _) = "PRED1"
  show (Boolean _) = "Bool"
  show (DET _) = "DET"
  show (ERR _) = "ERR"