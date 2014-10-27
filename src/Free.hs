{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Free where

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
  | Error String
  deriving Functor

instance Show (Step a) where
  show (Interact g) = "Interact"
  show (GetEnv g) = "Get"
  show (PutEnv e g) = "Put " ++ show e
  show (Get g) = "Get"
  show (Put e g) = "Put"
  show (Error s) = "Error: " ++ s

type Program = Free Step 

interp :: Env -> Dynamic -> [Event] -> Program a -> Either (Env, Dynamic, Program a) String
interp env ent events prog =
  case (prog, events) of
    (Impure (Interact g), e:es) -> interp env ent es (g e)
    (Impure (GetEnv g), _)      -> interp env ent events (g env)
    (Impure (PutEnv env' g), _) -> interp env' ent events (g ())
    (Impure (Get g), _)         -> interp env ent events (g ent)
    (Impure (Put d g), _)       -> interp env d events (g ())
    (Impure (Error s), _)       -> Right s
    (Pure r, _)               -> Left (env, ent, return r)
    otherwise                 -> Left (env, ent, prog)
-- ^ don't really like the way its pattern-matched

    -- hmm
liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

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

failure :: String -> Program a
failure s = liftF $ Error s

get :: Program Dynamic
get = liftF $ Get id

put :: Typeable a => a -> Program ()
put e = liftF $ Put d id
  where d = toDyn e

getEnv :: Program Env
getEnv = liftF $ GetEnv id

putEnv :: Env -> Program ()
putEnv env = liftF $ PutEnv env id

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
  forever $ do
    d <- collide
    when (fromDyn d "" == "a") $ do
      redraw rh (\ri -> ri { sign = 'x' })
      put $ toDyn "I'm dead"
      return ()

data Player = Player { health :: Int 
                     , playerX :: Int
                     , playerY :: Int
                     } deriving (Typeable, Show)

with :: Typeable a => (a -> Program b) -> Program b
with f = do
  en <- get
  case fromDynamic en of
    Just p -> f p
    Nothing -> failure "Wrong entity type"

whenEntity pred prog = with (\p -> when (pred p) prog)

player :: Program a
player = do
  rh <- with $ \Player { playerX = x, playerY = y } -> draw x y '@'
  forever $ do
    e <- interact
    whenEntity (\p -> (health p) > 0) $ do
      case e of
        Damage dmg -> do with $ \p -> put p { health = (health p) - dmg }
        Key key ->
          case key of
            'j' -> with $ \p -> do 
                                put p { playerY = (playerY p) - 1 }
                                redraw rh $ \ri -> ri { posY = (posY ri) - 1 }
            'k' -> with $ \p -> do
                                put p { playerY = (playerY p) + 1 }
                                redraw rh $ \ri -> ri { posY = (posY ri) + 1 }
            'h' -> with $ \p -> do
                                put p { playerX = (playerX p) - 1 }
                                redraw rh $ \ri -> ri { posX = (posX ri) - 1 }
            'l' -> with $ \p -> do
                                put p { playerX = (playerX p) + 1 }
                                redraw rh $ \ri -> ri { posX = (posX ri) + 1 }
            otherwise -> return ()
        otherwise -> return ()

--
-- this just mutates entity into object it collided with
prog2 :: Program a
prog2 = do
  forever $ do
    d <- collide
    put d

emptyEnv = Env { renderer = Renderer.init, entities = Entities.init }
entity = toDyn "Whatever"
playerEntity = toDyn Player { playerX = 0, playerY = 0, health = 100 }

-- prog succesfuly executed
Left (env, e, r) = interp emptyEnv entity [Collision $ toDyn "a"] prog
--
-- unchanged
Left (env', e', r') = interp emptyEnv entity [ Collision $ toDyn "w"
              , Collision $ toDyn "b"] prog

-- check if we'rfe eating events correctly
Left (env'', e'', r'') = interp emptyEnv entity [ Collision $ toDyn "brelam"
                               , Key 'a'
                               , Collision $ toDyn "a" ] prog

Left (env''', e''', r''') = interp emptyEnv entity [ Collision $ toDyn "brelam"
                                 , Collision $ toDyn "a" ] prog

Left (_, unchangedPos, _) = interp emptyEnv playerEntity [ Key 'j', Damage 10, Key 'k' ] player
p' = fromDyn unchangedPos Player {}

Left (_, changedPos, _) = interp emptyEnv playerEntity [ Key 'j', Damage 101, Key 'k' ] player
p'' = fromDyn changedPos Player {}

-- sequence
seqTest :: Program a
seqTest = forever $ do
  e <- interact
  case e of
    Key _ -> do
      e' <- interact
      case e' of
        Damage dmg -> do with $ \p -> put p { health = (health p) - dmg }
        _ -> return ()
    _ -> return ()

-- ohh
Left (_, sq', seqTest') = interp emptyEnv playerEntity [ Damage 10 ] seqTest
sqTest = fromDyn sq' Player {}
Left (_, sq'', seqTest'') = interp emptyEnv sq' [ Damage 10 ] seqTest'
sqTest' = fromDyn sq'' Player {}
Left (_, sq''', seqTest''') = interp emptyEnv sq'' [ Key 'a' ] seqTest''
sqTest'' = fromDyn sq' Player {}
Left (_, sq'''', seqTest'''') = interp emptyEnv sq''' [ Damage 10 ] seqTest'''
sqTest''' = fromDyn sq'''' Player {}
