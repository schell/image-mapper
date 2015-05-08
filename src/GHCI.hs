module GHCI where

import UI
import Scene
import Linear
import Data.Generic.Diff

--------------------------------------------------------------------------------
-- GHCI
--------------------------------------------------------------------------------
u0 = mkUi 0
u1 = mkUi 1
u2 = mkUi 2
u3 = mkUi 3
l1 = UILabel u1 "aoeu" zero zero
l2 = UILabel u2 "aoeu" zero zero
l3 = UILabel u3 "aoeu" zero zero
c0 = UIContainer u3 []
c1'  = UIContainer u0 [l1]
c2'' = UIContainer u0 [l1,l2]
c3''' = UIContainer u0 [c0,l1,l2]

--diff2_3 = editScene (diffElements [] [l1]) mempty
{-
diff2_3 corresponds to adding a new empty UIContainer to the head of the root
element's child element list, the diff produces this output:

Copy:    UIContainer
Copy:    UI
Copy:    Uid 0
Copy:    V2 0.0 0.0
Copy:    V2 0.0 0.0
Copy:    V2 1.0 1.0
Copy:    0.0
Copy:    [UIElement]
Insert:  UIContainer
Insert:  UI
Insert:  Uid 3
Insert:  V2 0.0 0.0
Insert:  V2 0.0 0.0
Insert:  V2 1.0 1.0
Insert:  0.0
Insert:  []
Insert:  [UIElement]
Copy:    UILabel
Copy:    UI
Copy:    Uid 1
Copy:    V2 0.0 0.0
Copy:    V2 0.0 0.0
Copy:    V2 1.0 1.0
Copy:    0.0
Copy:    "aoeu"
Copy:    V4 0.0 0.0 0.0 0.0
Copy:    V4 0.0 0.0 0.0 0.0
Copy:    [UIElement]
Copy:    UILabel
Copy:    UI
Copy:    Uid 2
Copy:    V2 0.0 0.0
Copy:    V2 0.0 0.0
Copy:    V2 1.0 1.0
Copy:    0.0
Copy:    "aoeu"
Copy:    V4 0.0 0.0 0.0 0.0
Copy:    V4 0.0 0.0 0.0 0.0
Copy:    []

Copy:    UIContainer
    Copy:    UI
        Copy:    Uid 0
        Copy:    V2 0.0 0.0
        Copy:    V2 0.0 0.0
        Copy:    V2 1.0 1.0
        Copy:    0.0
    Copy:    [UIElement]
        Insert:  UIContainer
            Insert:  UI
                Insert:  Uid 3
                Insert:  V2 0.0 0.0
                Insert:  V2 0.0 0.0
                Insert:  V2 1.0 1.0
                Insert:  0.0
    Insert:  []
    Insert:  [UIElement]
        Copy:    UILabel
        Copy:    UI
        Copy:    Uid 1
        Copy:    V2 0.0 0.0
        Copy:    V2 0.0 0.0
        Copy:    V2 1.0 1.0
        Copy:    0.0
        Copy:    "aoeu"
        Copy:    V4 0.0 0.0 0.0 0.0
        Copy:    V4 0.0 0.0 0.0 0.0
    Copy:    [UIElement]
        Copy:    UILabel
            Copy:    UI
                Copy:    Uid 2
                Copy:    V2 0.0 0.0
                Copy:    V2 0.0 0.0
                Copy:    V2 1.0 1.0
                Copy:    0.0
            Copy:    "aoeu"
            Copy:    V4 0.0 0.0 0.0 0.0
            Copy:    V4 0.0 0.0 0.0 0.0
    Copy:    []

-}
