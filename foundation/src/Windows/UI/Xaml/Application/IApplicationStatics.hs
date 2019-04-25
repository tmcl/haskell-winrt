module Windows.UI.Xaml.Application.IApplicationStatics where

import Windows.UI.Xaml.Application hiding (classId, iid)
import Windows.UI.Xaml.Application.IApplicationInitializationCallback hiding (iid)
-- import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams hiding (iid)
import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable hiding (iid)
import System.Windows.WinRT.IUnknown hiding (iid)
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.Lowlevel.Info
import Control.Monad.IO.Class

interfaceName :: String
interfaceName = "Windows.UI.Xaml.IApplicationStatics"

newtype Uri = Uri ()
instance Storable Uri where
   sizeOf _ = error "uri stub sizeof"
   alignment _ = error "uri stub alignment"
   peek _  = error "uri stub peek"
   poke _ _  = error "uri stub poke"

newtype ComponentResourceLocation = ComponentResourceLocation Int32
componentResourceLocationApplication :: ComponentResourceLocation
componentResourceLocationApplication = ComponentResourceLocation 0
componentResourceLocationNested :: ComponentResourceLocation
componentResourceLocationNested = ComponentResourceLocation 1

iid :: IID IApplicationStaticsVtbl 
iid = IID $ GUID 0x06499997 0xF7B4 0x45FE 0xB7 0x63 0x75 0x77 0xD1 0xD3 0xCB 0x4A

data IApplicationStaticsVtbl = IApplicationStaticsVtbl {
   inspectable :: IInspectableVtbl,
   cfp_get_Current :: FunPtr CurrentGetter,
   cfp_Start :: FunPtr Starter,
   cfp_LoadComponent :: FunPtr (Ptr IApplicationStatics → Ptr IInspectable → Ptr Uri → IO HRESULT),
   cfp_LoadComponentWithResourceLocation :: FunPtr (Ptr IApplicationStatics → Ptr IInspectable → Ptr Uri →ComponentResourceLocation → IO HRESULT)
}

instance IsUnknown IApplicationStaticsVtbl where
   fp_QueryInterface = fp_QueryInterface . inspectable

startApplication :: ApplicationStatics → ApplicationInitializationCallbackImpl → WinRT ()
startApplication statics cb = do 
   fp_cb ← liftIO $ newIUnknownImpl cb 
   passthrough (withForeignPtr fp_cb) $ \p_cb →
      withRuntimeInstance statics $ \vtbl p_statics →
         try $ mkStart (cfp_Start vtbl) p_statics (castPtr p_cb)


type CurrentGetter = Ptr IApplicationStatics → Ptr (Ptr IApplication) → IO HRESULT
foreign import ccall "dynamic"
      mkCurrentGetter :: FunPtr CurrentGetter → CurrentGetter

type Starter = Ptr IApplicationStatics → Ptr IApplicationInitializationCallback → IO HRESULT
foreign import  ccall "dynamic"
      mkStart :: FunPtr Starter → Starter

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