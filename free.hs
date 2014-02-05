module Main where

import Prelude hiding (interact)
import Data.String
import Control.Monad
import Control.Monad.Free
import Data.Dynamic
import Renderer hiding (draw, redraw)
import qualified Renderer
import Entities hiding (add, get, update)
import qualified Entities

data Env = Env { renderer :: RenderingEnv
               , entities :: Entities.EntitiesEnv }
               deriving Show

data Event = Collision Dynamic
           | Foo Int

data Step next =
    Interact (Event -> next)
  | Get (Env -> next)
  | Put Env (() -> next)

instance Functor Step where
  fmap f (Interact g)   = Interact (f . g)
  fmap f (Get g)        = Get (f . g)
  fmap f (Put e g)      = Put e (f . g)

instance Show (Step a) where
  show (Interact g) = "Interact"
  show (Get g) = "Get"
  show (Put e g) = "Put " ++ show e

type Program = Free Step

interp :: Env -> [Event] -> Program a -> (Env, Program a)
interp env events prog =
  case (prog, events) of
    (Free (Interact g), e:es) -> do
      interp env es (g e)
    (Free (Get g), _) -> do
      interp env events (g env)
    (Free (Put env' g), _) -> do
      interp env' events (g ())
    (Pure r, _) -> (env, return r)
    otherwise -> (env, prog)
-- ^ don't really like the way its pattern-matched

interact :: Program Event
interact = liftF (Interact id)

collide :: Program Dynamic
collide = do
  e <- interact
  case e of
    Collision d -> return d
    otherwise -> collide

getEnv :: Program Env
getEnv = liftF (Get id)

putEnv :: Env -> Program ()
putEnv env = liftF (Put env id)

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

updateEntity :: EntityHandle -> Maybe Dynamic -> Program ()
updateEntity h d = do
  env <- getEnv
  let e' = Entities.put (entities env) h d
  putEnv env { entities = e' }
  
draw :: Int -> Int -> Char -> Program RenderHandle
draw x y c = do
  env <- getEnv
  let (re, rh) = Renderer.draw (renderer env) RenderInfo { posX = x, posY = y, sign = c }
  putEnv env { renderer = re }
  return rh

redraw :: RenderHandle -> Int -> Int -> Char -> Program ()
redraw h x y c = do
  env <- getEnv
  let ri = RenderInfo { posX = x, posY = y, sign = c }
  let re = Renderer.redraw (renderer env) h (Just ri)
  putEnv env { renderer = re }

clear :: RenderHandle -> Program ()
clear h = do
  env <- getEnv
  putEnv env { renderer = Renderer.redraw (renderer env) h Nothing }

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
      redraw rh 1 1 'x'
      updateEntity en (Just $ toDyn "I'm dead")
      return ()

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
  en <- addEntity $ toDyn "whatever"
  forever $ do
    d <- collide
    updateEntity en (Just d)

emptyEnv = Env { renderer = Renderer.init, entities = Entities.init }
-- prog succesfuly executed
(env, r) = interp emptyEnv [Collision $ toDyn "a"] prog
--
-- unchanged
(env', r') = interp emptyEnv [ Collision $ toDyn "w"
              , Collision $ toDyn "b"] prog

-- check if we'rfe eating events correctly
(env'', r'') = interp emptyEnv [ Collision $ toDyn "brelam"
                               , Foo 1
                               , Collision $ toDyn "a" ] prog

(env''', r''') = interp emptyEnv [ Collision $ toDyn "brelam"
                                 , Collision $ toDyn "a" ] prog

