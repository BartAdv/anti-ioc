module Main where

import Data.String
import Control.Monad
import Control.Monad.Free
import Data.Dynamic
import Renderer hiding (draw, redraw)
import qualified Renderer

data Interaction next =
    Collide (Dynamic -> next)
  | Update Dynamic (() -> next)
  | Draw RenderInfo (RenderHandle -> next)
  | Redraw RenderHandle (Maybe RenderInfo) (() -> next)

instance Functor Interaction where
  fmap f (Collide g)    = Collide (f . g)
  fmap f (Update s g)   = Update s (f . g)
  fmap f (Draw r g)     = Draw r (f . g)
  fmap f (Redraw h r g) = Redraw h r (f . g)

instance Show (Interaction a) where
  show (Collide g) = "Collide"
  show (Update s g) = "Update " ++ (show $ fromDyn s "unk")
  show (Draw r g) = "Draw " ++ show r
  show (Redraw h r g) = "Redraw " ++ show h ++ " -> " ++ show r

type Program = Free Interaction

data Event = Collision Dynamic

data Env = Env { renderer :: RenderingEnv } deriving Show

interp :: Env -> Dynamic -> [Event] -> Program a -> (Env, Program a, Dynamic)
interp env ent e prog =
  case (prog, e) of
    (Free (Collide g), (Collision d:es)) -> do
      interp env ent es (g d)
    (Free (Update d g), _) -> do
      interp env d e (g ())
    (Free (Draw r g), _) -> do
      let (renv, h) = Renderer.draw (renderer env) r
      interp env { renderer = renv } ent e (g h)
    (Free (Redraw h r g), _) -> do
      interp env { renderer = Renderer.redraw (renderer env) h r } ent e (g ())
    (Pure r, _) -> (env, return r, ent)
    otherwise -> (env, prog, ent)
-- ^ don't really like the way its pattern-matched

collide :: Program Dynamic
collide = liftF (Collide id)

update :: Dynamic -> Program ()
update d = liftF (Update d id)

draw :: Int -> Int -> Char -> Program RenderHandle
draw x y c = liftF (Draw RenderInfo { posX = x, posY = y, sign = c } id)

redraw :: RenderHandle -> Int -> Int -> Char -> Program ()
redraw h x y c = liftF (Redraw h (Just RenderInfo { posX = x, posY = y, sign = c }) id)

clear :: RenderHandle -> Program ()
clear h = liftF (Redraw h Nothing id)

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
