module Windows.UI.Xaml.Application.IApplicationInitializationCallback where

import Foreign
import Control.Monad
import System.Windows.GUID
import System.Windows.WinRT.IUnknown
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.Info
import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams

-- interfaceName :: String
-- interfaceName = "Windows.UI.Xaml.IApplicationInitializationCallback"

type Invoker = Ptr IApplicationInitializationCallbackParams → IO HRESULT

iid :: IID IApplicationInitializationCallbackVtbl 
iid = IID $ GUID 0xB6351C55 0xC284 0x46E4
   0x83 0x10 0xfB 0x09 0x67 0xFA 0xB7 0x6F

data IApplicationInitializationCallbackVtbl = IApplicationInitializationCallbackVtbl {
   iunknown :: IUnknownVtbl,
   cfp_Invoke :: FunPtr Invoker
}
instance Storable IApplicationInitializationCallbackVtbl where
   sizeOf it = sizeOfMethodPtr + sizeOf (iunknown it)
   alignment _ = sizeOfMethodPtr
   peekByteOff ptr off = do
      iunknown ← peekByteOff ptr off
      cfp_Invoke ← peekByteOff ptr (off + sizeOf iunknown)
      return $ IApplicationInitializationCallbackVtbl {..}
   pokeByteOff ptr off IApplicationInitializationCallbackVtbl {..} = do
      pokeByteOff ptr off iunknown
      pokeByteOff ptr (off + sizeOf iunknown) cfp_Invoke

type IApplicationInitializationCallback = Ptr IApplicationInitializationCallbackVtbl

data ApplicationInitializationCallback = ApplicationInitializationCallback {
   foreignPtr :: ForeignPtr IApplicationInitializationCallback,
   vtable :: IApplicationInitializationCallbackVtbl
}

data MyLocalInfo = MyLocalInfo

data Something = Something {
   appl_init_callback_vtable :: IApplicationInitializationCallbackVtbl,
   appl_init_data :: MyUnknown MyLocalInfo
}
instance Storable Something where
   sizeOf it = sizeOf (appl_init_callback_vtable it) + 2 * sizeOfPtr
   alignment _ = sizeOfMethodPtr
   peekByteOff ptr off = do
      let vtablePtr = ptr `plusPtr` (2 * sizeOfPtr)
      storedStuff ← peekByteOff ptr (off + sizeOfPtr)
      vtablePtr2 ← peekByteOff ptr off
      unless (vtablePtr2 == vtablePtr `plusPtr` off) $ error "vtablePtr2 and vtablePtr were different, so i read gibberish"
      appl_init_callback_vtable ←peekByteOff vtablePtr off
      appl_init_data ← deRefStablePtr $ castPtrToStablePtr storedStuff
      return Something {..}
   pokeByteOff ptr off Something{..} = do
      let vtablePtr = ptr `plusPtr` (2 * sizeOfPtr)
      storedStuff ← castStablePtrToPtr <$> newStablePtr appl_init_data
      pokeByteOff ptr off vtablePtr
      pokeByteOff ptr (off + sizeOfPtr) storedStuff
      pokeByteOff vtablePtr off appl_init_callback_vtable



-- newtype LocalApplicationInitializationCallback = LocalApplicationInitializationCallback {
--    internal_vtable :: IApplicationInitializationCallbackVtbl
--    internal_data :: 
-- }
-- instance Storable LocalApplicationInitializationCallback where
--    sizeOf _ = sizeOfPtr + sizeOfMethodPtr + sizeOf (undefined :: IApplicationInitializationCallbackVtbl)
--    alignment = sizeOfMethodPtr
--    peekByteOff ptr off = do
-- 
--       return 
-- 
-- new :: (Ptr IApplicationInitializationCallbackParams → HRESULT) → WinRT ApplicationInitializationCallback
-- new cb = do
--    mallocForeignPtr 