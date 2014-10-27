module Entities where

import Data.Map as M
import Data.Dynamic

type EntityHandle = Int

type EntitiesEnv = Map EntityHandle Dynamic

add :: EntitiesEnv -> Dynamic -> (EntitiesEnv, EntityHandle)
add env e = (insert idx e env, idx)
  where idx = size env

put :: EntitiesEnv -> EntityHandle -> Maybe Dynamic -> EntitiesEnv
put env h e = update (const e) h env

get :: EntitiesEnv -> EntityHandle -> Maybe Dynamic
get env h = M.lookup h env

init :: EntitiesEnv
init = empty

