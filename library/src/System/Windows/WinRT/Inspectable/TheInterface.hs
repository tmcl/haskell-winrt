{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}

module System.Windows.WinRT.Inspectable.TheInterface (
   Inspectable(..), 
   IInspectable, 
   IInspectableVtbl(..), 
   QueryInterfaceType,
   UnsafeQueryInterfaceType,
   GetIidsType,
   TrustLevel
   )
where

import Foreign
import Foreign.C.Types
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.IUnknown
import System.Windows.WinRT.Lowlevel.HSTRING
import System.Windows.WinRT.Lowlevel.Info

data TrustLevel

data Inspectable = Inspectable { 
   insp_foreignPtr :: ForeignPtr IInspectable,
   insp_vtable :: IInspectableVtbl
}

type IInspectable = Ptr IInspectableVtbl



type GetIidsType = Ptr IInspectable → Ptr CULong → Ptr (Ptr GUID) → IO HRESULT
data IInspectableVtbl = IInspectableVtbl {
   iunknown :: IUnknownVtbl,

   cfp_GetIids :: FunPtr GetIidsType,
   cfp_GetRuntimeClassName :: FunPtr (Ptr IInspectable → Ptr HSTRING → IO HRESULT),
   cfp_GetTrustLevel :: FunPtr (Ptr IInspectable → Ptr TrustLevel → IO HRESULT)
}

instance Storable IInspectableVtbl where
   sizeOf _ = sizeOf (undefined :: IUnknownVtbl) + 3 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 
   peekByteOff ptr off = do
      iunknown ← peekByteOff ptr off
      cfp_GetIids ← peekByteOff ptr (off + 3 * sizeOfMethodPtr)
      cfp_GetRuntimeClassName ← peekByteOff ptr (off + 4 * sizeOfMethodPtr)
      cfp_GetTrustLevel ← peekByteOff ptr (off + 5 * sizeOfMethodPtr)
      return IInspectableVtbl {..}

   pokeByteOff ptr off IInspectableVtbl {..} = do
      pokeByteOff ptr off iunknown
      pokeByteOff ptr (off + 3 * sizeOfMethodPtr) cfp_GetIids
      pokeByteOff ptr (off + 4 * sizeOfMethodPtr) cfp_GetRuntimeClassName
      pokeByteOff ptr (off + 5 * sizeOfMethodPtr) cfp_GetTrustLevel

-- withIInspectable :: Inspectable → (Ptr IInspectable → WinRT a) → WinRT a
-- withIInspectable Inspectable{..} = passthrough $ withForeignPtr insp_foreignPtr 