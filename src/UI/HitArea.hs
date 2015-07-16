{-# LANGUAGE DeriveGeneric #-}
module UI.HitArea where

import Types.Internal
import Types.Renderable
import Linear hiding (outer)
import Gelatin.Core.Rendering hiding (triangulate)
import Gelatin.Core.Color
--import Gelatin.Core.Triangulation.EarClipping
import Data.Typeable
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.IntMap as IM

instance Renderable HitArea where
    nameOf _ = "HitArea"
    cache (Rez geom _ _ win _) rs p@(HitArea _ vs clr o) = do
        let --ts = {-map (\(a,b,c) -> Triangle a b c) $ -}triangulate vs
            inner = polyline EndCapRound LineJoinBevel 2 vs
            outline = outlinePolyline EndCapRound LineJoinBevel 2 vs
            outer = polyline EndCapRound LineJoinBevel 1 outline
            --fill = solid clr

        --Rendering fpoly cpoly <- filledTriangleRendering win geom ts fill
        l <- if o then do r1 <- filledTriangleRendering win geom inner $ solid white
                          r2 <- filledTriangleRendering win geom outer $ solid black
                          return $ r1 `mappend` r2
                  else return mempty

        let --r = Rendering f cpoly `mappend` l
            --f t = do stencilMask (fpoly t) (fpoly t)

        return $ IM.insert (hash p) l{-r-} rs
    renderLayerOf p = [(hash p, Just $ polyTransform p)]

instance Hashable HitArea where
    hashWithSalt s (HitArea _ ps c o) =
        s `hashWithSalt` ps `hashWithSalt` c `hashWithSalt` o

data HitArea = HitArea { polyTransform  :: Transform
                       , polyPoints     :: [V2 Float]
                       , polyColor      :: Color
                       , polyIsOutlined :: Bool
                       } deriving (Show, Eq, Typeable, Generic)
