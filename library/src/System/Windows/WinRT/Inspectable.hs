{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- todo remove me


module System.Windows.WinRT.Inspectable (
   module System.Windows.WinRT.Inspectable.TheInterface,
   module System.Windows.WinRT.Inspectable.QueryInterface,
   module System.Windows.WinRT.Inspectable.NewInspectable,
   iid
)
where

import System.Windows.GUID
import System.Windows.WinRT.Inspectable.TheInterface
import System.Windows.WinRT.Inspectable.QueryInterface
import System.Windows.WinRT.Inspectable.NewInspectable

iid :: IID ()
iid = IID $ GUID 
   0xAF86E2E0 0xB12D 0x4c6a 
   0x9C 0x5A 0xD7 0xAA 0x65 0x10 0x1E 0x90
