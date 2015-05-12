{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Scene where

import Prelude hiding (sequence_)
import UI
import Control.Lens hiding (transform)
import Data.Generic.Diff
import Gelatin.Core.Render hiding (Transform, el)
import Control.Concurrent.Async
import Graphics.GL.Core33
import Control.Concurrent
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Data.Bits
import Data.Typeable
import System.Exit

data Rez = Rez { rezGeom      :: GeomRenderSource
               , rezBez       :: BezRenderSource
               , rezWindow    :: Window
               , rezFontCache :: Async FontCache
               } deriving (Typeable)

type MakesScene r = (Member (Reader Rez) r, SetMember Lift (Lift IO) r)

data UIStack = UIStack { _stackIns  :: [Uid]
                       , _stackDels :: [Uid]
                       , _stackCpys :: [Uid]
                       , _stackPrvs :: [Uid] -- ^ Parents
                       , _stackCur  :: Uid
                       , _stackCurCanParent :: Bool
                       } deriving (Typeable, Show)
makeLenses ''UIStack

data Update = Create Uid UIElement
            | Destroy Uid
            | Rerender Uid UIElement
            | Transform Uid UITransform
            deriving (Show)

type EffDiff r = (SetMember Lift (Lift IO) r,
                  Member (State UIStack) r,
                  Member (State [Update]) r)

data EffPatchResult = EffPatchResult

space :: Int -> String
space i = concat $ replicate i "  "

spacePrint :: (SetMember Lift (Lift IO) r, Member (State [Int]) r) => String -> Eff r ()
spacePrint s = do
    (is :: [Int]) <- get
    lift $ putStrLn $ space (sum is) ++ s

editScene :: UserInterface -> UserInterface -> IO [Update]
editScene t t' = do
    putStrLn "\n"
    let edit = diff t t'
    runLift $ evalState (UIStack [] [] [] [] 0 False)
            $ evalState ([] :: [Update])
            $ performEdits edit

setCurrent :: EffDiff r => Uid -> Eff r ()
setCurrent u = do
    modify $ stackCur .~ u
    lift $ putStrLn $ " set current " ++ show u
    printCurrent

pushParent :: EffDiff r => Eff r ()
pushParent = do
    u <- current
    lift $ putStrLn $ "   push parent " ++ show u
    modify $ stackPrvs %~ (u:)

popParent :: EffDiff r => Eff r (Maybe Uid)
popParent = do
    ps <- _stackPrvs <$> get
    case ps of
        p:ps' -> do modify $ stackPrvs .~ ps'
                    setCurrent p
                    lift $ putStrLn $ "   pop parent " ++ show p
                    return $ Just p
        _     -> return Nothing

del :: EffDiff r => Uid -> Eff r ()
del = modify . (stackDels %~) . (:)

ins :: EffDiff r => Uid -> Eff r ()
ins = modify . (stackIns %~) . (:)

cpy :: EffDiff r => Uid -> Eff r ()
cpy = modify . (stackCpys %~) . (:)

current :: EffDiff r => Eff r Uid
current = _stackCur <$> get

parent :: EffDiff r => Eff r (Maybe Uid)
parent = do
    ps <- _stackPrvs <$> get
    return $ case ps of
        p:_ -> Just p
        _   -> Nothing

printCurrent :: EffDiff r => Eff r ()
printCurrent = do
    mp <- parent
    c  <- current
    let extra = case mp of
                    Just p -> " (Parent is " ++ show p ++ ")"
                    _      -> ""
    lift $ putStrLn $ "Current is " ++ show c ++ extra

isElement :: UIFamily x y -> UIFamily x y -> Bool
isElement (UILabel' _) (UILabel' _) = True
isElement _ _ = False

isTfrm :: UIFamily x y -> UIFamily x y -> Bool
isTfrm (UITfrm' _) (UITfrm' _) = True
isTfrm _ _ = False

rerender :: EffDiff r => UIElement -> Eff r ()
rerender el = do
    u <- current
    modify $ (++ [Rerender u el])

transform :: EffDiff r => UITransform -> Eff r ()
transform t = do
    u <- current
    modify $ (++ [Transform u t])

performEdits :: EffDiff r => EditScriptL UIFamily x y -> Eff r [Update]
performEdits (Cpy c (Cpy (Uid' u) es)) = do
    lift $ putStrLn $ "Copy " ++ string c ++ " (" ++ show u ++ ")"
    setCurrent u
    cpy u
    performEdits es
performEdits (Cpy c (Ins (Uid' u) es)) = do
    lift $ putStrLn $ "Insert " ++ string c ++ " (" ++ show u ++ ")"
    setCurrent u
    ins u
    performEdits es
performEdits (Cpy c (Del (Uid' u) es)) = do
    lift $ putStrLn $ "Insert " ++ string c ++ " (" ++ show u ++ ")"
    setCurrent u
    del u
    performEdits es
performEdits (Cpy UICons' es) = do
    lift $ putStrLn $ "Copy " ++ string UICons'
    pushParent
    performEdits es
performEdits (Cpy UINil' es) = do
    lift $ putStrLn $ "Copy " ++ string UINil'
    _ <- popParent
    performEdits es
performEdits (Cpy c es) = do
    lift $ putStrLn $ "Copy " ++ string c
    performEdits es

performEdits (Ins (UILabel' u) (Del (UILabel' _) es)) = do
    lift $ putStrLn $ "Rerender label"
    rerender u
    performEdits es
performEdits (Ins (UITfrm' t) (Del (UITfrm' _) es)) = do
    lift $ putStrLn $ "Transform"
    transform t
    performEdits es
performEdits (Ins c es) = do
    lift $ putStrLn $ "Insert " ++ string c
    performEdits es

performEdits (Del c es) = do
    lift $ putStrLn $ "Delete " ++ string c
    performEdits es

performEdits (CpyTree es) = do
    --spacePrint "CopyTree"
    lift $ putStrLn $ "CopyTree"
    performEdits es

performEdits End = do
    (stack :: UIStack) <- get
    lift $ print stack
    get >>= return

stepScene :: Window -> Scene -> IO ()
stepScene win scene = do
    (fbw,fbh) <- getFramebufferSize win
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    renderScene scene

    pollEvents
    swapBuffers win
    shouldClose <- windowShouldClose win
    if shouldClose
    then exitSuccess
    else threadDelay 100

renderScene :: Scene -> IO ()
renderScene _ = undefined --sequence_ $ IM.intersectionWith rRender rs ts

mkSceneFromUI :: UIElement -> IO Scene
mkSceneFromUI = undefined
