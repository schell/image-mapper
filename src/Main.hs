{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (sequence_)
import Render
import Entity
import Linear
import Graphics.Text.TrueType
import Control.Applicative
import Control.Monad hiding (sequence_)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Data.Monoid
import Data.Maybe
import Data.Foldable (sequence_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type Colors = IntMap Color
type Transforms = IntMap Transform
type ParentEntities = IntMap UniqueId
type Displays = IntMap Display
type Mutates a = Member (State a)
type MakesIds = Member (Fresh UniqueId)
type DoesIO = SetMember Lift (Lift IO)

transforms :: (Mutates Transforms r,
               Mutates ParentEntities r,
               Mutates Displays r)
           => Eff r Transforms
transforms = do
    ts <- get
    parents <- get
    let displayMap = fmap findParents parents
        findParents p = allParents parents p ++ [p]
        parentTfrms = fmap (foldr mappend mempty . catMaybes . fmap findTfrms) displayMap
        findTfrms = flip IM.lookup ts . unId
    return $ IM.unionWith mappend ts parentTfrms

-- | Lists a branch of all parents, root first.
allParents :: ParentEntities -> UniqueId -> [UniqueId]
allParents parents uid =
    -- Precaution so we don't recurse if a parent contains itself
    takeWhile (/= uid) allParents'
    where allParents' = case IM.lookup (unId uid) parents of
                            Nothing -> []
                            Just uid' -> allParents parents uid' ++ [uid']

main :: IO ()
main = do
    putStrLn "aoeusnth"
    rsrcs <- initResources 800 600 "Syndeca Mapper"
    runLift $ evalState (IM.empty :: Transforms)
            $ evalState (IM.empty :: ParentEntities)
            $ evalState (IM.empty :: Displays)
            $ evalState (IM.empty :: Colors)
            $ evalState rsrcs
            $ flip runFresh (UniqueId 0)
            $ app

app :: (Mutates ParentEntities r,
        Mutates Transforms r,
        Mutates Colors r,
        Mutates Displays r,
        Mutates Resources r,
        MakesIds r,
        DoesIO r)
    => Eff r ()
app = do
    enableBlending

    entity ## Transform (V2 0 60) (V2 1 1) 0
           ## (Color $ V4 1 0 0 1)
           .# DisplayText' ubuntuMono{_descriptorStyle = FontStyle True False} 128 "{}/~Test test test"

    button <- entity ## Transform (V2 100 100) (V2 1 1) 0
                     ## (Color $ V4 0.5 0.5 0.5 1)
                     ## DisplayPoly [V2 0 0, V2 100 0, V2 100 40, V2 0 40]

    entity `inside` button
           ## Transform (V2 10 32) (V2 1 1) 0
           ## (Color $ V4 1 1 1 1)
           ## DisplayText ubuntuMono 32 "Start"
           .# button

    forever $ withNewFrame $ do
        drawClear
        ts <- transforms
        ds <- get
        cs <- (fmap unColor) <$> get
        sequence_ $ intersectionWithKey3 drawThing cs ds ts
