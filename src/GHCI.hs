module GHCI where

import UI
import Scene
import Linear
import Data.Generic.Diff

--------------------------------------------------------------------------------
-- GHCI
--------------------------------------------------------------------------------
rerenderLabel :: (UserInterface, UserInterface)
rerenderLabel = (a,b)
    where a = UIBranch 0 mempty [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero]
                                , UILeaf 3 mempty $ UILabel "second" zero zero
                                ]
          b = UIBranch 0 mempty [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first-changed" zero zero]
                                , UILeaf 3 mempty $ UILabel "second" zero zero
                                ]

createLeafDeleteLeaf :: (UserInterface, UserInterface)
createLeafDeleteLeaf = (a,b)
    where a = UIBranch 0 mempty [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ]
                                , UILeaf 3 mempty $ UILabel "third" zero zero
                                ]
          b = UIBranch 0 mempty [ UIBranch 1 mempty [ UILeaf 2 mempty $ UILabel "first" zero zero, UILeaf 3 mempty $ UILabel "second" zero zero ]
                                , UILeaf 4 mempty $ UILabel "third" zero zero
                                ]
