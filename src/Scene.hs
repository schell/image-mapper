{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Scene where

import Prelude hiding (sequence_)
import Types
import Network
import Linear
import Gelatin.Core.Render
import Graphics.GL.Types
import Graphics.Text.TrueType
import Codec.Picture
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Hashable
import Data.Monoid
import Data.Maybe
import Data.List (intercalate)
import Data.Text (pack)
import qualified Data.IntMap as IM

renderElement :: (MakesScene r, StatsRenderers r) => Element -> Eff r ()
renderElement (Element a) = do
    r <- getRenderer a
    lift $ (rRender r) (transformOf a)

getRenderer :: (StatsRenderers r, MakesScene r, Renderable a, Hashable a, Show a)
            => a -> Eff r Renderer
getRenderer a
    | [] <- childElementsOf a = do
        let h' = hash a
        setUsed h'
        mAR <- lookupAttached h'
        case mAR of
            Nothing -> newRenderer h' a
            Just r  -> return r
    | otherwise = do
        let es  = childElementsOf a
        rs <- mapM getRenderer es
        let ts  = map transformOf es
            rs' = zipWith transformRenderer ts rs
        return $ mconcat rs'
--------------------------------------------------------------------------------
-- Renderables
--------------------------------------------------------------------------------
instance Renderable Network where
    render (Network _ _) = return Nothing
    transformOf = const mempty
    childElementsOf (Network m l) = [Element m, Element l]

instance Renderable MappingScreen where
    render (MappingScreen _ _ _ _) = return Nothing
    transformOf = mappingScreenTfrm
    childElementsOf (MappingScreen _ bg p (MSInfo ia ib)) =
        [Element bg, Element p, Element ia, Element ib]

instance Renderable Picture where
    render (Pic _ fp w h) = do
        eStrOrImg <- lift $ readImage fp
        case eStrOrImg of
            Left err -> lift $ putStrLn err >> return Nothing
            Right i  -> do tx <- lift $ loadTexture i
                           r  <- uiPic tx w h
                           return $ Just r
    transformOf = picTransform

instance Renderable Box where
    render (Box _ sz c) = uiBox sz c >>= return . Just
    transformOf = boxTransform

instance Renderable Label where
    render (Label _ str fn ps fc) = do
        afc <- rezFontCache <$> ask
        mefc <- lift $ poll afc
        case mefc of
            Just (Right cache) -> uiLabel cache str fn ps fc >>= return . Just
            _                  -> return Nothing
    transformOf = labelTransform

instance Renderable Button where
    render (Button t box label) = do
        mb <- render box
        ml <- render label
        return $ do b <- mb
                    l <- ml
                    return $ (transformRenderer t b) <> (transformRenderer t l)
    transformOf = buttonTransform

instance Renderable Element where
    render (Element a)          = render a
    transformOf (Element a)     = transformOf a
    childElementsOf (Element a) = childElementsOf a

instance Renderable a => Renderable [a] where
    render es = do rs <- catMaybes <$> mapM render es
                   return $ Just $ mconcat rs
    transformOf _ = mempty

instance Renderable a => Renderable (Maybe a) where
    render (Just a) = render a
    render _        = return Nothing
    transformOf (Just a) = transformOf a
    transformOf _        = mempty

instance Renderable () where
    render () = return $ Just mempty
    transformOf = mempty

class Renderable a where
    transformOf   :: a -> Transform
    render        :: MakesScene r => a -> Eff r (Maybe Renderer)
    childElementsOf :: a -> [Element]
    childElementsOf _ = []
--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------
instance Hashable Element where
    hashWithSalt s (Element a) = hashWithSalt s a

instance Show Element where
    show (Element a) = "Element{ " ++ show a ++ " }"

data Element where
    Element  :: (Show a, Renderable a, Hashable a) => a -> Element
--------------------------------------------------------------------------------
-- Handling Renderers as Resources
--------------------------------------------------------------------------------
attach :: (Member (State AttachedRenderers) r, SetMember Lift (Lift IO) r)
       => Int -> Renderer -> Eff r ()
attach i a' = do
    mR <- lookupAttached i
    case mR of
        Nothing -> return ()
        Just r -> lift $ rCleanup r
    modify $ \(Attached rs) -> Attached $ IM.insert i a' rs

setUsed :: (Member (State UsedThisFrame) r) => Int -> Eff r ()
setUsed i = modify $ \(UsedThisFrame rs) -> UsedThisFrame $ IM.insertWith (+) i 1 rs

lookupAttached :: (Member (State AttachedRenderers) r)
               => Int -> Eff r (Maybe Renderer)
lookupAttached i = (IM.lookup i . attached) <$> get

dropUnusedRenderers :: (StatsRenderers r, ModifiesRenderers r, DoesIO r) => Eff r ()
dropUnusedRenderers = do
    Attached rs <- get
    UsedThisFrame ts <- get

    let toClean = IM.difference rs ts
        clean = (lift . rCleanup) <$> toClean
        left  = IM.difference rs toClean

    _ <- sequence clean

    put $ UsedThisFrame mempty
    put $ Attached left
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- | Create a new renderer for an element and attaches it with the given
-- UID. If the renderer cannot be created return a no-op and attach
-- nothing.
newRenderer :: (Renderable a, MakesScene r, Hashable a)
            => Int -> a -> Eff r Renderer
newRenderer i el = do
    mr <- render el
    r <- case mr of
        Just r  -> attach i r >> return r
        Nothing -> return mempty
    return r

uiPic :: MakesScene r => GLuint -> Int -> Int -> Eff r Renderer
uiPic tx w' h' = do
    Rez grs _ win _ <- ask
    let [w,h] = map fromIntegral [w',h']
        tl = V2 0 0
        tr = V2 w 0
        bl = V2 0 h
        br = V2 w h
        vs = [tl, tr, bl, tr, bl, br]
        gs = map (/ V2 w h) vs
    lift $ textureRenderer win grs tx GL_TRIANGLES vs gs

uiLabel :: MakesScene r
        => FontCache -> String -> String -> PointSize -> V4 Float
        -> Eff r Renderer
uiLabel cache str fn ps fc = do
    Rez grs brs w _ <- ask
    let desc = FontDescriptor (pack fn) $ FontStyle False False
    mRend <- lift $ withFont cache desc $ \font -> do
        -- | TODO: figure out why the bounding box is too low
        let px = getPointSize ps
            fs = FontString font px (0, px) str
        colorFontRenderer w grs brs fs (const fc)
    case mRend of
        Nothing -> do lift $ do putStrLn $ "Could not find " ++ show desc
                                let fnts = enumerateFonts cache
                                    fstr = intercalate "\n    " $ map show fnts
                                putStrLn $ "Available:\n" ++ fstr
                      return mempty
        Just r  -> return r

uiBox :: MakesScene r => Size -> V4 Float -> Eff r Renderer
uiBox (V2 w h) c = do
    Rez geom _ win _ <- ask
    let [tl, tr, br, bl] = [zero, V2 w 0, V2 w h, V2 0 h]
        vs = [tl, tr, br, tl, br, bl]
        cs = replicate 6 c
    lift $ colorRenderer win geom GL_TRIANGLES vs cs

