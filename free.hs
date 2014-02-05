module Main where

import Data.String
import Control.Monad
import Control.Monad.Free
import Data.Dynamic
import Renderer hiding (draw, redraw)
import qualified Renderer

data Env = Env { renderer :: RenderingEnv } deriving Show

data Interaction next =
    Collide (Dynamic -> next)
  | Update Dynamic (() -> next)
  | Get (Env -> next)
  | Put Env (() -> next)

instance Functor Interaction where
  fmap f (Collide g)    = Collide (f . g)
  fmap f (Update s g)   = Update s (f . g)
  fmap f (Get g)        = Get (f . g)
  fmap f (Put e g)      = Put e (f . g)

instance Show (Interaction a) where
  show (Collide g) = "Collide"
  show (Update s g) = "Update " ++ (show $ fromDyn s "unk") -- this will most likely be eliminated
  show (Get g) = "Get"
  show (Put e g) = "Put " ++ show e

type Program = Free Interaction

data Event = Collision Dynamic


interp :: Env -> Dynamic -> [Event] -> Program a -> (Env, Program a, Dynamic)
interp env ent e prog =
  case (prog, e) of
    (Free (Collide g), (Collision d:es)) -> do
      interp env ent es (g d)
    (Free (Update d g), _) -> do
      interp env d e (g ())
    (Free (Get g), _) -> do
      interp env ent e (g env)
    (Free (Put env' g), _) -> do
      interp env' ent e (g ())
    (Pure r, _) -> (env, return r, ent)
    otherwise -> (env, prog, ent)
-- ^ don't really like the way its pattern-matched

collide :: Program Dynamic
collide = liftF (Collide id)

update :: Dynamic -> Program ()
update d = liftF (Update d id)

get :: Program Env
get = liftF (Get id)

put :: Env -> Program ()
put env = liftF (Put env id)

draw :: Int -> Int -> Char -> Program RenderHandle
draw x y c = do
  env <- get
  let (re, rh) = Renderer.draw (renderer env) RenderInfo { posX = x, posY = y, sign = c }
  put env { renderer = re }
  return rh

redraw h x y c = do
  env <- get
  let ri = RenderInfo { posX = x, posY = y, sign = c }
  let re = Renderer.redraw (renderer env) h (Just ri)
  put env { renderer = re }

{-
redraw :: RenderHandle -> Int -> Int -> Char -> Program ()
redraw h x y c = liftF (Redraw h (Just RenderInfo { posX = x, posY = y, sign = c }) id)

clear :: RenderHandle -> Program ()
clear h = liftF (Redraw h Nothing id)
-}

{-
- usage
-}

-- this is just program for entities that 'die' on a collision
prog :: Program a
prog = do
  rh <- draw 1 1 '*'
  forever $ do
    d <- collide
    when (fromDyn d "" == "a") $ do
      redraw rh 1 1 'x'
      update $ toDyn "I'm dead"
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
  forever $ do
    d <- collide
    update d

e1 = toDyn "Entity 1"
e2 = toDyn "Entity 2"

emptyEnv = Env { renderer = Renderer.init }
-- prog succesfuly executed
(env, r,e) = interp emptyEnv e1 [Collision $ toDyn "a"] prog
--
-- unchanged
(env', r',e') = interp emptyEnv e1 [ Collision $ toDyn "w"
              , Collision $ toDyn "b"] prog

-- check if we'rfe eating events correctly
(env'', r'', e'') = interp emptyEnv e1 [ Collision $ toDyn "brelam"
                                       , Collision $ toDyn "a" ] prog

(env''', r''', e''') = interp emptyEnv e1 [ Collision $ toDyn "brelam"
                                       , Collision $ toDyn "a" ] prog
