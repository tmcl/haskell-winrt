module Windows.UI.Xaml.Application.IApplicationStatics where

import qualified Windows.UI.Xaml.Application
import Windows.UI.Xaml.Application hiding (classId, iid)
import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams hiding (iid)
import Foreign
import System.Windows.GUID
import System.Windows.WinRT.HString
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown hiding (iid)
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.Lowlevel.Info

interfaceName :: String
interfaceName = "Windows.UI.Xaml.IApplicationStatics"

newtype Uri = Uri ()
instance Storable Uri where
   sizeOf _ = error "uri stub sizeof"
   alignment _ = error "uri stub alignment"
   peek _  = error "uri stub peek"
   poke _ _  = error "uri stub poke"

newtype ComponentResourceLocation = ComponentResourceLocation Int32
componentResourceLocationApplication = 0
componentResourceLocationNested = 1

iid :: IID IApplicationStaticsVtbl 
iid = IID $ GUID 0x06499997 0xF7B4 0x45FE 0xB7 0x63 0x75 0x77 0xD1 0xD3 0xCB 0x4A

data IApplicationStaticsVtbl = IApplicationStaticsVtbl {
   inspectable :: IInspectableVtbl,
   cfp_get_Current :: FunPtr (Ptr (Ptr IApplication) → HRESULT),
   cfp_Start :: FunPtr (Ptr IApplicationInitializationCallbackParams → HRESULT),
   cfp_LoadComponent :: FunPtr (Ptr IInspectable → Ptr Uri → HRESULT),
   cfp_LoadComponentWithResourceLocation :: FunPtr (Ptr IInspectable → Ptr Uri →ComponentResourceLocation → HRESULT)
}

inspectableSize :: Int
inspectableSize = sizeOf (undefined :: IInspectableVtbl)

instance Storable IApplicationStaticsVtbl where
   sizeOf _ = inspectableSize + 4 * sizeOfMethodPtr
   alignment _ = alignment (undefined :: IInspectableVtbl)
   peekByteOff ptr off = do
      inspectable ← peekByteOff ptr off
      cfp_get_Current ← peekByteOff ptr (0 * sizeOfMethodPtr + inspectableSize + off)
      cfp_Start ← peekByteOff ptr (1 * sizeOfMethodPtr + inspectableSize + off) 
      cfp_LoadComponent ← peekByteOff ptr (2 * sizeOfMethodPtr + inspectableSize + off)
      cfp_LoadComponentWithResourceLocation ← peekByteOff ptr (3 * sizeOfMethodPtr + inspectableSize + off)
      return IApplicationStaticsVtbl {..}
   poke ptr IApplicationStaticsVtbl{..} = do
      pokeByteOff ptr 0 inspectable
      pokeByteOff ptr (0 * sizeOfMethodPtr + inspectableSize) cfp_get_Current
      pokeByteOff ptr (1 * sizeOfMethodPtr + inspectableSize) cfp_Start
      pokeByteOff ptr (2 * sizeOfMethodPtr + inspectableSize) cfp_LoadComponent
      pokeByteOff ptr (3 * sizeOfMethodPtr + inspectableSize) cfp_LoadComponentWithResourceLocation

instance ActivatableFactory IApplicationStaticsVtbl where
   af_iid = iid
   -- af_classId = Windows.UI.Xaml.Application.classId

type IApplicationStatics = Ptr IApplicationStaticsVtbl

type ApplicationStatics = RuntimeInstance IApplicationStaticsVtbl