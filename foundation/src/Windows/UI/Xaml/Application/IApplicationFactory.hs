{-# LANGUAGE RecursiveDo #-} 
module Windows.UI.Xaml.Application.IApplicationFactory where

import Windows.UI.Xaml.Application hiding (classId, iid)
import Windows.UI.Xaml.Application.IApplicationOverrides hiding (iid, inspectable)
import qualified Windows.UI.Xaml.Application.IApplicationOverrides
import Windows.UI.Xaml.Application.IApplicationOverrides_impl
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
interfaceName = "Windows.UI.Xaml.IApplicationFactory"

iid :: IID IApplicationFactoryVtbl 
iid = IID $ GUID 0x93BBE361 0xBE5A 0x4EE3 0xB4 0xA3 0x95 0x11 0x8D 0xC9 0x7A 0x89

data IApplicationFactoryVtbl = IApplicationFactoryVtbl {
   inspectable :: IInspectableVtbl,
   cfp_CreateInstance :: FunPtr InstanceCreator
}

instance IsUnknown IApplicationFactoryVtbl where
   fp_QueryInterface = fp_QueryInterface . inspectable

type InstanceCreator = 
   Ptr IApplicationFactory 
   → Ptr IApplicationOverrides
   → Ptr (Ptr IInspectable)
   → Ptr (Ptr IApplication)
   → IO HRESULT

createApplication :: ApplicationFactory 
   → IApplicationOverridesDetail 
   → WinRT Application
createApplication factory overrides = mdo 
   let overridesImpl = extend base overrides
   fp_overrides ← liftIO $ newIUnknownImpl overridesImpl
   (base, appl) ← passthrough (withForeignPtr fp_overrides) $ \p_overrides →
      alloca2wrt $ \pp_out_base pp_out_appl →
         withRuntimeInstance factory $ \vtbl p_factory → do
            try $ mkCreate (cfp_CreateInstance vtbl) p_factory (castPtr p_overrides) pp_out_base pp_out_appl
            p_out_base ← liftIO $ peek pp_out_base
            p_out_appl ← liftIO $ peek pp_out_appl
            p_base_vtbl ← liftIO $ peek p_out_base
            p_appl_vtbl ← liftIO $ peek p_out_appl
            base_vtbl ← liftIO $ peek p_base_vtbl
            appl_vtbl ← liftIO $ peek p_appl_vtbl
            fp_out_base ← liftIO $ newForeignPtr_ p_out_base
            fp_out_appl ← liftIO $ newForeignPtr_ p_out_appl
            let appl' = RuntimeInstance fp_out_appl appl_vtbl 
            let base'insp = RuntimeInstance fp_out_base base_vtbl
            base' ← queryInterface Windows.UI.Xaml.Application.IApplicationOverrides.iid base'insp
            return (base', appl')
   return appl

foreign import  ccall "dynamic"
      mkCreate :: FunPtr InstanceCreator → InstanceCreator

instance Storable IApplicationFactoryVtbl where
   sizeOf _ = inspectableSize + sizeOfMethodPtr
   alignment _ = alignment (undefined :: IInspectableVtbl)
   peekByteOff ptr off = do
      inspectable ← peekByteOff ptr off
      cfp_CreateInstance ← peekByteOff ptr (0 * sizeOfMethodPtr + inspectableSize + off)
      return IApplicationFactoryVtbl {..}
   poke ptr IApplicationFactoryVtbl{..} = do
      pokeByteOff ptr 0 inspectable
      pokeByteOff ptr (0 * sizeOfMethodPtr + inspectableSize) cfp_CreateInstance

instance ActivatableFactory IApplicationFactoryVtbl where
   af_iid = iid
   -- af_classId = Windows.UI.Xaml.Application.classId

type IApplicationFactory = Ptr IApplicationFactoryVtbl

type ApplicationFactory = RuntimeInstance IApplicationFactoryVtbl