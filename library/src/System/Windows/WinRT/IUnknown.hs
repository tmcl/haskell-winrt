module System.Windows.WinRT.IUnknown
where

import Foreign
import Foreign.C
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.Info

-- interfaceName :: String
-- interfaceName = "Windows.UI.Xaml.IApplicationInitializationCallback"

type InstanceOf menu = Ptr menu
data RuntimeInstance menu = RuntimeInstance {
    foreignPtr :: ForeignPtr (InstanceOf menu),
    vtable :: menu
}

-- withIInspectable :: Inspectable → (Ptr IInspectable → WinRT a) → WinRT a
-- withIInspectable Inspectable{..} act = passthrough . withForeignPtr insp_foreignPtr $ act

withRuntimeInstance :: RuntimeInstance menu → (menu → Ptr (InstanceOf menu) → WinRT a) → WinRT a
withRuntimeInstance RuntimeInstance{..} action = 
    passthrough (withForeignPtr foreignPtr) (action vtable)

iid :: IID IUnknownVtbl 
iid = IID $ GUID 0x00000000 0x0000 0x0000
   0xC0 0x00 0x00 0x00 0x00 0x00 0x00 0x46

type REFIID = Ptr GUID
type UnsafeQueryInterfaceType = Ptr IUnknown → REFIID → Ptr (Ptr ()) → IO HRESULT
type QueryInterfaceType b = Ptr IUnknown → Ptr (IID b) → Ptr (Ptr (Ptr b)) → IO HRESULT

data IUnknownVtbl = IUnknownVtbl {
   cfp_QueryInterface :: FunPtr UnsafeQueryInterfaceType,
   cfp_AddRef :: FunPtr (Ptr IUnknown → IO CULong),
   cfp_Release :: FunPtr (Ptr IUnknown → IO ()) -- actually a HRESULT, but I need to run it in a finaliser
}

type IUnknown = Ptr IUnknownVtbl

type Unknown = RuntimeInstance IUnknownVtbl

instance Storable IUnknownVtbl where
   sizeOf _ = 3 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 

   peekByteOff ptr off = do
      cfp_QueryInterface ← peekByteOff ptr (off + 0 * sizeOfMethodPtr)
      cfp_AddRef ← peekByteOff ptr (off + 1 * sizeOfMethodPtr)
      cfp_Release ← peekByteOff ptr (off + 2 * sizeOfMethodPtr)
      return IUnknownVtbl {..}

   pokeByteOff ptr off IUnknownVtbl {..} = do
      pokeByteOff ptr (off + 0 * sizeOfMethodPtr) cfp_QueryInterface
      pokeByteOff ptr (off + 1 * sizeOfMethodPtr) cfp_AddRef
      pokeByteOff ptr (off + 2 * sizeOfMethodPtr) cfp_Release
