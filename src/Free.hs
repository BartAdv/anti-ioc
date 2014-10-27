{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}

module Free where

import Prelude hiding (interact)
import Data.String
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Free
import qualified Control.Monad.State as ST
import Data.Dynamic
import Renderer hiding (draw, redraw)
import qualified Renderer
import Entities hiding (add, get, update, put)
import qualified Entities

import Debug.Trace

data Event = Collision Dynamic
           | Rendered RenderHandle
           | Key Char
           | Damage Int
           deriving Show
                      
data Command = Render RenderInfo
             | Redraw RenderHandle (RenderInfo -> RenderInfo)
             | Clear RenderHandle
               
instance Show Command where
  show (Render ri) = "render: " ++ (show ri)
  show (Redraw rh _) = "redraw " ++ (show rh)
  show (Clear rh) = "clear: " ++ (show rh)

data Step next = Await (Event -> next)
               | Order Command (() -> next)
               | Get (Dynamic -> next)
               | Put Dynamic (() -> next)
               | Error String
               deriving Functor

instance Show (Step a) where
  show (Await g) = "Await"
  show (Order c g) = "Order: " ++ (show c)
  show (Get g) = "Get"
  show (Put e g) = "Put"
  show (Error s) = "Error: " ++ s

type Program = Free Step

interp :: Program a -> [Event] -> Dynamic -> Either (Program a, [Command], Dynamic) String
interp = interp' []
  where
    interp' cs (Impure (Await g))   (e:es) ent = interp' cs (g e) es ent
    interp' cs (Impure (Order c g)) es ent = interp' (c:cs) (g ()) es ent
    interp' cs (Impure (Get g))     es     ent = interp' cs (g ent) es ent
    interp' cs (Impure (Put d g))   es     ent = interp' cs (g ()) es d
    interp' cs (Impure (Error s))   _      _   = Right s
    interp' cs (Pure r)             _      ent = Left (return r, cs, ent)
    interp' cs prog                 _      ent = Left (prog, cs, ent)

printp :: Program a -> String
printp (Impure s) = show s
printp (Pure _) = "pure"

    -- hmm
liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

await :: Program Event
await = liftF (Await id)

order :: Command -> Program ()
order cmd = liftF $ Order cmd id

get :: Program Dynamic
get = liftF $ Get id

put :: Typeable a => a -> Program ()
put e = liftF $ Put d id
  where d = toDyn e
        
failure :: String -> Program a
failure s = liftF $ Error s

collide :: Program Dynamic
collide = do
  e <- await
  case e of
    Collision d -> return d
    otherwise -> collide

waitForKey :: Program Char
waitForKey = do
  e <- await
  case e of
    Key c -> return c
    otherwise -> waitForKey
-- ^^ some common pattern with not that straightforward wy to be refactored
-- out
    
draw :: Int -> Int -> Char -> Program RenderHandle
draw x y c = do
  order $ Render RenderInfo { posX = x, posY = y, sign = c }
  loop
    where loop = do
            ev <- await
            case trace (show ev) $ ev of
              Rendered rh -> return rh
              _ -> loop -- notice that we effectively ditch every event till we got what we wanted, this could be changed

redraw :: RenderHandle -> (RenderInfo -> RenderInfo) -> Program ()
redraw h upd = order $ Redraw h upd

clear :: RenderHandle -> Program ()
clear h = order $ Clear h

{-
- usage
-}

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
    e <- await
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

entity = toDyn "Whatever"
playerEntity = toDyn Player { playerX = 0, playerY = 0, health = 100 }

test :: [Event] -> ST.StateT (Program a, Dynamic) IO ()
test es = do
  (prog, ent) <- ST.get
  let Left (prog', cs, ent') = interp prog es ent
      sh = fromDyn ent' Player{}
  ST.liftIO $ putStrLn $ (printp prog') ++ "\n" ++ (show cs) ++ "\n" ++ (show sh)
  ST.liftIO $ putStrLn ""
  ST.put (prog', ent')

tests :: ST.StateT (Program a, Dynamic) IO ()
tests = do
  test []
  test [Rendered 1]
  test [Key 'j']
  test [Key 'j']
  return ()

main :: IO ()
main = do
       _ <- ST.runStateT tests (player, playerEntity)
       return ()

  
