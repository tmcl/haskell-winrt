{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- todo remove me


module System.Windows.WinRT.Inspectable.NewInspectable (newInspectable)
where

import Foreign
import System.Windows.WinRT.Inspectable.TheInterface
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.HSTRING
import Control.Monad.IO.Class

foreign import ccall "RoActivateInstance"
    c_RoActivateInstance :: HSTRING → Ptr (Ptr IInspectable) → IO HRESULT

newInspectable :: HString → WinRT Inspectable
newInspectable = flip withHString $ \h_className → 
   wrap . alloca $ \(pp_inspApp :: Ptr (Ptr IInspectable)) → unwrap $ do
      try $ c_RoActivateInstance h_className pp_inspApp
      liftIO $ do
         p_inspApp ← peek pp_inspApp
         inspApp ← peek p_inspApp
         insp_vtable ← peek inspApp
         let IInspectableVtbl {..} = insp_vtable
         insp_foreignPtr ← newForeignPtr_ p_inspApp
         -- insp_foreignPtr ← newForeignPtr c_Release p_inspApp
         return Inspectable {..}