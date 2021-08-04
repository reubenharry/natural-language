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
import Types
    ( BaseTree(..),
      IdiomInterpreter,
      Interpreter,
      M,
      Sem(..),
      World,
      Entity(..) ) 
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
import Data.Void ( Void )

exampleTree :: Fix (BaseTree String)
exampleTree = Fix $ Branch [
  Fix $ Branch [
    Fix $ Leaf "the",
    Fix $ Leaf "woman"],
  Fix $ Branch[
    Fix $ Leaf "is",
    Fix $ Branch [
      Fix $ Leaf "a",
      Fix $ Leaf "runner"]
    ]]

exampleWorld :: World
exampleWorld = [
  (John, [
    ("runner", "True")]),
  (Jill, [
    ("runner", "True")])]
  
exampleInterpretation :: Either String Bool
exampleInterpretation =
  let unpack = runM exampleWorld . getSentence 
      meaning = histo semantics 
  in unpack $ meaning exampleTree




semantics :: IdiomInterpreter Void (Sem M)
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

  Word "the" -> QUANT $ \x -> do 
    w <- asks (fmap fst)
    y <- filterM x w
    case y of
      [uniqueEntity] -> return uniqueEntity
      xs -> throwError ("presup failure: " <> show xs)

  Word "everyone" -> E $ do 
    dom <- asks (fmap fst)
    ContT $ \f -> foldr1 (liftA2 (&&)) $ fmap f dom

  Word "someone" -> E $ do
    dom <- asks (fmap fst)
    ContT $ \f -> foldr1 (liftA2 (||)) $ fmap f dom

  Word x -> ERR ("LexicalError: " <> show x)

  Branch [
    _ :< Word "runs",
    _ :< Word "wild"
    ] -> PRED1 $ \x -> do
      w <- ask
      attrs <- case lookup x w of 
        Nothing -> throwError "Nothing runs"
        Just y -> return y
      return (lookup "runner" attrs == Just "True")

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

  Composition (E str) (PRED1 vp) -> BOOLEAN $ vp =<< str
  Composition (QUANT det) (PRED1 n) -> E $ det n
  Composition (ERR a) (ERR b) -> ERR (a <> " " <> b)
  Composition (ERR a) _ -> ERR a
  Composition _ (ERR b) -> ERR b

  Branch [x :< _, y :< _] -> ERR (show (x,y)) -- "no interpretation for this"   
  _ -> ERR "not implemented"     


linearize :: Interpreter String
linearize = \case

  Leaf x -> x
  Branch [r1, r2]  -> r1 <> " " <> r2
  _ -> "Cannot interpret partial trees"

-----------
-- patterns
-----------

pattern Composition :: a -> a -> BaseTree leafType (Cofree f a)
pattern Composition r1 r2 <- (Branch [r1 :< _, r2 :< _])

pattern Word :: leafType -> BaseTree leafType branchType
pattern Word str <- Leaf str

--------------------
-- word meanings
--------------------
run :: Entity -> M Bool
run x = do

  w <- ask
  attrs <- case lookup x w of 
    Nothing -> throwError "Nothing runs"
    Just y -> return y
  return $ lookup "runner" attrs == Just "True"


--------------------
-- utls
--------------------

getSentence :: MonadError String a => Sem a -> a Bool
getSentence (BOOLEAN b) = b
getSentence (ERR s) = throwError s
getSentence _ = throwError "error"


runM :: r -> ContT Bool (ReaderT r (ExceptT e Identity)) Bool -> Either e Bool
runM w x = runIdentity $ runExceptT $ flip runReaderT w $ runContT x return


-- an instance to ensure that adding ContT to the stack preserves the ability to throw errors
instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

instance Show (Sem a) where
  show (E _) = "E"
  show (PRED1 _) = "PRED1"
  show (BOOLEAN _) = "Bool"
  show (QUANT _) = "DET"
  show (ERR _) = "ERR"