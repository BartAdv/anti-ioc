{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Prelude hiding (interact)
import Data.String
import Control.Monad
import Control.Monad.Free
import Data.Dynamic
import Renderer hiding (draw, redraw)
import qualified Renderer
import Entities hiding (add, get, update, put)
import qualified Entities

data Env = Env { renderer :: RenderingEnv
               , entities :: Entities.EntitiesEnv }
               deriving Show

data Event = Collision Dynamic
           | Key Char
           | Damage Int

data Step next =
    Interact (Event -> next)
  | GetEnv (Env -> next)
  | PutEnv Env (() -> next)
  | Get (Dynamic -> next)
  | Put Dynamic (() -> next)

instance Functor Step where
  fmap f (Interact g)   = Interact (f . g)
  fmap f (GetEnv g)     = GetEnv (f . g)
  fmap f (PutEnv e g)   = PutEnv e (f . g)
  fmap f (Get g)        = Get (f . g)
  fmap f (Put d g)      = Put d (f . g)

instance Show (Step a) where
  show (Interact g) = "Interact"
  show (GetEnv g) = "Get"
  show (PutEnv e g) = "Put " ++ show e
  show (Get g) = "Get"
  show (Put e g) = "Put"

type Program = Free Step

interp :: Env -> Dynamic -> [Event] -> Program a -> (Env, Dynamic, Program a)
interp env ent events prog =
  case (prog, events) of
    (Free (Interact g), e:es) -> do
      interp env ent es (g e)
    (Free (GetEnv g), _) -> do
      interp env ent events (g env)
    (Free (PutEnv env' g), _) -> do
      interp env' ent events (g ())
    (Free (Get g), _) -> do
      interp env ent events (g ent)
    (Free (Put d g), _) -> do
      interp env d events (g ())
    (Pure r, _) -> (env, ent, return r)
    otherwise -> (env, ent, prog)
-- ^ don't really like the way its pattern-matched

interact :: Program Event
interact = liftF (Interact id)

collide :: Program Dynamic
collide = do
  e <- interact
  case e of
    Collision d -> return d
    otherwise -> collide

waitForKey :: Program Char
waitForKey = do
  e <- interact
  case e of
    Key c -> return c
    otherwise -> waitForKey
-- ^^ some common pattern with not that straightforward wy to be refactored
-- out

get :: Program Dynamic
get = liftF (Get id)

put :: Dynamic -> Program ()
put d = liftF (Put d id)

getEnv :: Program Env
getEnv = liftF (GetEnv id)

putEnv :: Env -> Program ()
putEnv env = liftF (PutEnv env id)

getEntity :: EntityHandle -> Program (Maybe Dynamic)
getEntity h = do
  env <- getEnv
  return $ Entities.get (entities env) h

addEntity :: Dynamic -> Program EntityHandle
addEntity d = do
  env <- getEnv
  let (e', h) = Entities.add (entities env) d
  putEnv env { entities = e' }
  return h

updateEntity :: EntityHandle -> Dynamic -> Program ()
updateEntity h d = do
  env <- getEnv
  let e' = Entities.put (entities env) h (Just d)
  putEnv env { entities = e' }

removeEntity :: EntityHandle -> Program ()
removeEntity h = do
  env <- getEnv
  let e' = Entities.put (entities env) h Nothing
  putEnv env { entities = e' }

draw :: Int -> Int -> Char -> Program RenderHandle
draw x y c = do
  env <- getEnv
  let (re, rh) = Renderer.draw (renderer env) RenderInfo { posX = x, posY = y, sign = c }
  putEnv env { renderer = re }
  return rh

redraw :: RenderHandle -> (RenderInfo -> RenderInfo) -> Program ()
redraw h upd = do
  env <- getEnv
  let re = Renderer.redraw (renderer env) h (\ri -> Just $ upd ri)
  putEnv env { renderer = re }

clear :: RenderHandle -> Program ()
clear h = do
  env <- getEnv
  putEnv env { renderer = Renderer.redraw (renderer env) h (const Nothing) }

{-
- usage
-}

-- this is just program for entities that 'die' on a collision
prog :: Program a
prog = do
  rh <- draw 1 1 '*'
  en <- addEntity $ toDyn "Entity"
  forever $ do
    d <- collide
    when (fromDyn d "" == "a") $ do
      redraw rh (\ri -> ri { sign = 'x' })
      updateEntity en $ toDyn "I'm dead"
      return ()

data Player = Player { health :: Int } deriving Typeable

{-
whenPlayer :: (Player -> Bool) -> Program () -> Program ()
whenPlayer pred prog = do
  en <- get
  case fromDynamic en of
    Just p -> when (pred p) prog
    Nothing -> return ()
-}

withEntity :: Typeable a => (a -> Program ()) -> Program ()
withEntity f = do
  en <- get
  case fromDynamic en of
    Just p -> f p
    Nothing -> return ()

whenEntity pred prog = withEntity (\p -> when (pred p) prog)

player :: Int -> Int -> Program a
player x y = do
  rh <- draw x y '@'
  forever $ do
    e <- interact
    whenEntity (\p -> health p > 0) $ do
      case e of
        Damage dmg -> do
          withEntity (\p -> put $ toDyn p { health = (health p) - dmg })
        Key key ->
          case key of
            'j' -> redraw rh (\ri -> ri { posY = (posY ri) - 1 } )
            'k' -> redraw rh (\ri -> ri { posY = (posY ri) + 1 } )
            'h' -> redraw rh (\ri -> ri { posX = (posX ri) - 1 } )
            'l' -> redraw rh (\ri -> ri { posX = (posX ri) + 1 } )
            otherwise -> return ()
        otherwise -> return ()

-- as above, without loop
{-
prog' :: Program a
prog' = do
  d <- collide
  when (fromDyn d "" == "a") $ do
    update $ toDyn "I'm dead"
-}
--
-- this just mutates entity into object it collided with
prog2 :: Program a
prog2 = do
  forever $ do
    d <- collide
    put d

emptyEnv = Env { renderer = Renderer.init, entities = Entities.init }
entity = toDyn "Whatever"
playerEntity = toDyn Player { health = 100 }

-- prog succesfuly executed
(env, e, r) = interp emptyEnv entity [Collision $ toDyn "a"] prog
--
-- unchanged
(env', e', r') = interp emptyEnv entity [ Collision $ toDyn "w"
              , Collision $ toDyn "b"] prog

-- check if we'rfe eating events correctly
(env'', e'', r'') = interp emptyEnv entity [ Collision $ toDyn "brelam"
                               , Key 'a'
                               , Collision $ toDyn "a" ] prog

(env''', e''', r''') = interp emptyEnv entity [ Collision $ toDyn "brelam"
                                 , Collision $ toDyn "a" ] prog

