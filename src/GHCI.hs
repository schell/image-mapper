module GHCI where

import UI
import Scene
import Linear
import Data.Generic.Diff
import Data.Tuple

--------------------------------------------------------------------------------
-- GHCI
--------------------------------------------------------------------------------
showDiff :: (UserInterface, UserInterface) -> IO ()
showDiff = putStrLn . diffString

diffString :: (UserInterface, UserInterface) -> String
diffString = unlines . map show . scriptToList . uncurry diff

createLabel :: (UserInterface, UserInterface)
createLabel = (a, b)
    where a = []
          b = [UILeaf 0 mempty $ UILabel "label" zero zero]

deleteLabel :: (UserInterface, UserInterface)
deleteLabel = swap createLabel

createTwoLabels :: (UserInterface, UserInterface)
createTwoLabels = (a, b)
    where a = []
          b = [ UILeaf 0 mempty $ UILabel "label" zero zero , UILeaf 1 mempty $ UILabel "label" zero zero ]

rerenderLabel :: (UserInterface, UserInterface)
rerenderLabel = (a,b)
    where a = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero]
              , UILeaf 3 mempty $ UILabel "second" zero zero
              ]
          b = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first-changed" zero zero]
              , UILeaf 3 mempty $ UILabel "second" zero zero
              ]

createLeafDeleteLeaf :: (UserInterface, UserInterface)
createLeafDeleteLeaf = (a,b)
    where a = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ]
              , UILeaf 4 mempty $ UILabel "third" zero zero
              ]
          b = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ]
              , UILeaf 5 mempty $ UILabel "third" zero zero
              ]

deleteParentMoveChild :: (UserInterface, UserInterface)
deleteParentMoveChild = (a, b)
    where a = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero ] ]
          b = [ UILeaf 2 mempty $ UILabel "first" zero zero ]

deleteParentMoveChildren :: (UserInterface, UserInterface)
deleteParentMoveChildren = (a, b)
    where a = [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ] ]
          b = [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ]
