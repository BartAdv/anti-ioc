module Renderer where

import Data.Map

data RenderInfo = RenderInfo { posX :: Int
                             , posY :: Int
                             , sign :: Char }
                             deriving Show
type RenderHandle = Int

type RenderingEnv = Map RenderHandle RenderInfo

draw :: RenderingEnv -> RenderInfo -> (RenderingEnv, RenderHandle)
draw env r =
    let idx = size env 
    in (insert idx r env, idx)

redraw :: RenderingEnv -> RenderHandle -> (RenderInfo -> Maybe RenderInfo) -> RenderingEnv
redraw env h upd = update upd h env

init :: RenderingEnv
init = empty

