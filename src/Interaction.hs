{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Interaction where

import qualified Control.Comonad.Trans.Cofree as FT
import Control.Monad (when)
import Pragmatics (informativeSpeaker, listener, prior)
import Reactive.Banana as R (accumB, compile, (<@), (<@>))
import Reactive.Banana.Frameworks as R
  ( AddHandler,
    MomentIO,
    actuate,
    fromAddHandler,
    newAddHandler,
    reactimate,
  )
import System.IO (hFlush, stdout)
import Types
  ( CAT (S),
    Entity (Jane, Jill, John),
    IdiomGrammar,
    IdiomInterpreter,
    M,
    NoPausing,
    Sem (ERR),
    World,
  )
import Utl (fromFree, normalForm2)

speak :: World -> IO ()
speak speakerWorld = do
  putStrLn "\nThis is the world that the speaker is trying to convey\n"
  print speakerWorld
  putStrLn "\nPress Return to compute the speaker's next utterance\n"
  (handler, ev) <- newAddHandler
  network <- compile $ realTimeLanguage handler speakerWorld
  actuate network
  repl ev

repl :: (() -> IO a) -> IO ()
repl ev = loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      s <- getLine
      when (s /= "quit") (ev () >> loop)

realTimeLanguage :: AddHandler () -> World -> MomentIO ()
realTimeLanguage ev speakerWorld = mdo
  -- recursive do notation

  speechMoment <- fromAddHandler ev
  -- the following are mutually recursive
  let u = fst . head . normalForm2 <$> ((flip informativeSpeaker <$> belief) <@> (speakerWorld <$ speechMoment))
  belief <- accumB prior (listener <$> u)

  let world = (fmap fst . normalForm2 <$> belief) <@ u

  reactimate (putStrLn "\nThese are all the possible worlds, according to the listener:\n" <$ u)
  reactimate (mapM_ (printSelectingTrueWorld speakerWorld) <$> world)
  reactimate (printUtterance <$> u)

printSelectingTrueWorld :: (Show a1, Show a2) => a2 -> a1 -> IO ()
printSelectingTrueWorld w w' =
  let str = show w'
   in if str == show w
        then putStrLn $ str <> "      <------------------------ TRUE WORLD"
        else putStrLn str

printUtterance :: [Char] -> IO ()
printUtterance utt =
  putStrLn $
    if utt == ""
      then "\nThe speaker's best course of action is to say nothing"
      else "\nAnd this is the optimal utterance by the speaker:\n\n" <> utt <> "\n\nPress Return to compute the speaker's next utterance\n"
