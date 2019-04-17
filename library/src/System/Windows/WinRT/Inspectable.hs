{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- todo remove me


module System.Windows.WinRT.Inspectable (
   module System.Windows.WinRT.Inspectable.TheInterface,
   module System.Windows.WinRT.Inspectable.QueryInterface,
   module System.Windows.WinRT.Inspectable.NewInspectable
)
where

import System.Windows.WinRT.Inspectable.TheInterface
import System.Windows.WinRT.Inspectable.QueryInterface
import System.Windows.WinRT.Inspectable.NewInspectable

-- new :: (IsInspectable a) => InspectableInfo a → WinRT a
-- new runtimeClass = do
--    let my_className = className runtimeClass
--    insp ← newInspectable my_className
--    out :: a ← queryInterface (iid runtimeClass) insp
--    return out
