{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantics where

import Control.Applicative
import Control.Arrow ((&&&), (***))
import qualified Control.Comonad.Cofree as F
import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad.Bayes.Class (MonadSample)
import Control.Monad.Bayes.Enumerator ()
import Control.Monad.Bayes.Sampler ()
import Control.Monad.Bayes.Traced ()
import Control.Monad.Bayes.Weighted ()
import qualified Control.Monad.Free as F
import Control.Monad.State (MonadState (get))
import qualified Control.Monad.Trans.Free as FT
import Data.Functor.Compose (Compose (Compose))
import qualified Data.Functor.Foldable as Fold
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Void (Void)
import Syntax ()
import Types (BaseTree (Branch, Leaf), CAT, FragmentInterpreter, Grammar, Interpreter)
import Utl ()
import Prelude hiding (Word, words)
import Data.Functor
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Except
import qualified Data.Map as M

-- import FragmentGrammar
-- import Data.Map

data Entity = Jane | John | Jill deriving (Show, Eq, Ord)

type World = M.Map Entity Attrs

type Attrs = M.Map String String

data Sem a
  = E (a Entity)
  | Pred1 (Entity -> a Bool)
  | Boolean (a Bool)
  | DET ((Entity -> a Bool) -> a Entity)
  | ERR String

instance Show (Sem a) where
  show (E _) = "E"
  show (Pred1 _) = "Pred1"
  show (Boolean _) = "Bool"
  show (DET _) = "DET"
  show (ERR _) = "ERR"

newtype Logger = Logger String deriving (Show, Ord, Eq)

instance Semigroup Logger where
  (Logger a) <> (Logger b) = Logger (a <> " " <> b) 

instance Monoid Logger where
  mempty = Logger ""

getSentence :: Sem a -> Either String (a Bool) 
getSentence (Boolean b) = Right b
getSentence (ERR s) = Left s
getSentence _ = Left "error"

type M = ContT Bool (ReaderT World (ExceptT String Identity))

-- an instance to ensure that adding ContT to the stack preserves the ability to throw errors
instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

woman Jane = True
woman Jill = True
woman John = False

semanticsApple :: FragmentInterpreter Identity pauseType (Sem M)
-- semanticsApple :: (Monad m, Show pauseType) => Interpreter m pauseType (String, World -> Bool)
semanticsApple (Compose x) = do
  x' <- x
  case x' of



    Word "John" -> return $ E $ pure John
    Word "Jane" -> return $ E $ pure Jane
    Word "Jill" -> return $ E $ pure Jill
    Word "runs" -> return $ Pred1 $ \x -> do
      
      w <- ask
      attrs <- case M.lookup x w of 
        Nothing -> throwError "error: presup"
        Just y -> return y

      return $ M.lookup "runner" attrs == Just "True"

    Word "woman" -> return $ Pred1 $ pure . woman

    Word "the" -> return $ DET $ \x -> do 
      w <- asks M.keys
      y <- filterM x w
      case y of
        [theBlah] -> return theBlah
        xs -> throwError ("presup failure: " <> show xs)
      -- ContT $ \f -> foldr1 (liftA2 (&&)) $ fmap f dom


    Word "everyone" -> return $ E $ do 
      dom <- asks M.keys
      ContT $ \f -> foldr1 (liftA2 (&&)) $ fmap f dom

    Word "someone" -> return $ E $ do
      dom <- asks M.keys 
      ContT $ \f -> foldr1 (liftA2 (||)) $ fmap f dom

    Word x -> return $ ERR x -- return $ E $ throwError $ show x
    
    (FT.Free (Branch 
      [Identity (E e1) F.:< _,
       _ F.:< Compose (Identity (FT.Free (Branch 
        [_ F.:< (Compose (Identity (Word "and"))),
         (Identity (E e2)) F.:< _
         ])))
      ])) -> Identity $ E $ do
        e1' <- e1
        e2' <- e2
        ContT $ \f -> liftA2 (&&) (f e1') (f e2')

    Composition r1 r2  -> do
      r1' <- r1
      r2' <- r2
      case (r1',r2') of
          (E str, Pred1 vp) -> return $ Boolean $ vp =<< str -- undefined -- liftA2 (,) undefined (liftA2 (&&) r1 r2)
          (DET det, Pred1 n) -> return $ E $ det n
          (ERR a, ERR b) -> return $ ERR (a <> " " <> b)
          (ERR a, _) -> return $ ERR a
          (_, ERR a) -> return $ ERR a

          (a,b) -> error $ show (a,b)

    _ -> return $ ERR "partial"        


linearize :: FragmentInterpreter Identity pauseType String
linearize (Compose x) = do
  x' <- x
  case x' of


    Word x -> return x
    
    Composition r1 r2  -> do
      r1' <- r1
      r2' <- r2
      return (r1' <> " " <> r2')


    _ -> return "partial"   

pattern Composition r1 r2 <- FT.Free (Branch [r1 F.:< Compose _, r2 F.:< Compose _])
pattern Word str <- FT.Free (Leaf str)


type Lexicon m pauseType resultType = (Interpreter m pauseType resultType, Grammar m pauseType CAT)
