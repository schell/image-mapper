{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Subpub where

import Data.IORef
import Data.HMap
import Data.Dependent.Map
import Control.Monad.State.Strict

data Tag a where
    String' :: Tag String
    Int'    :: Tag Int
    IO'     :: Tag (IO b)

data P m b = P { run :: m b
               , cur :: b
               , sub :: DMap Tag
               }

type Signal m b = StateT (Stack b) m ()

data Stack b = Stack { stackStore :: a
                     , stackSave  :: b
                     , stackSubs  :: Signal b
                     }


getlineNPub :: _ IO Int String
getlineNPub = do
    str <- getLine
    n   <- unstore
    store $ n + 1
    send $ str ++ show (n+1)
