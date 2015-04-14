{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Entity where

import Prelude hiding (init)
import Data.Typeable
import qualified Data.IntMap as IM
--import qualified Data.Map as M

newtype UniqueId = UniqueId { unId :: Int } deriving (Show, Enum, Eq, Typeable)

--hasProperty :: (Member (State (IM.IntMap a)) r, Typeable a) => UniqueId -> a -> Eff r ()
--hasProperty eid val = modify $ IM.insert (unId eid) val
--
--entity :: (Member (Fresh a) r, Enum a, Typeable a) => Eff r a
--entity = fresh
--
--(##) :: (Member (State (IM.IntMap a)) r, Typeable a) => Eff r UniqueId -> a -> Eff r UniqueId
--f ## prop = do
--   eid <- f
--   modify $ IM.insert (unId eid) prop
--   return eid
--
--(.#) :: (Member (State (IM.IntMap a)) r, Typeable a) => Eff r UniqueId -> a -> Eff r ()
--f .# prop = do
--   eid <- f
--   modify $ IM.insert (unId eid) prop
--   return ()
--
--inside :: (Member (State (IM.IntMap UniqueId)) r)
--       => Eff r UniqueId -> UniqueId -> Eff r UniqueId
--inside = (##)
--
--inside_ :: (Member (State (IM.IntMap UniqueId)) r)
--        => Eff r UniqueId -> UniqueId -> Eff r ()
--inside_ = (.#)
--
--named :: (Member (State (M.Map k UniqueId)) r, Typeable k, Ord k)
--      => Eff r UniqueId -> k -> Eff r UniqueId
--named f n = do
--    eid <- f
--    modify $ M.insert n eid
--    return eid
--
--named_ :: (Member (State (M.Map k UniqueId)) r, Typeable k, Ord k) => Eff r UniqueId -> k -> Eff r ()
--named_ f n = named f n >> return ()
--
--
--getEntityBy :: (Member (State (M.Map k a)) r, Typeable a, Typeable k, Ord k) => k -> Eff r (Maybe a)
--getEntityBy name = fmap (M.lookup name) get

--------------------------------------------------------------------------------
-- Intersections
--------------------------------------------------------------------------------
intersectionWith3 f a b c = IM.intersectionWith ($) (IM.intersectionWith f a b) c
intersectionWith4 f a b c d = IM.intersectionWith ($) (intersectionWith3 f a b c) d
intersectionWith5 f a b c d e = IM.intersectionWith ($) (intersectionWith4 f a b c d) e

intersectionWithKey3 f a b c = IM.intersectionWith ($) (IM.intersectionWithKey f a b) c
intersectionWithKey4 f a b c d = IM.intersectionWith ($) (intersectionWithKey3 f a b c) d
intersectionWithKey5 f a b c d e = IM.intersectionWith ($) (intersectionWithKey4 f a b c d) e
