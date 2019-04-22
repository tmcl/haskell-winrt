module Windows.UI.Xaml.Application.IApplicationOverrides where

import Windows.UI.Xaml.Application hiding (classId, iid)
import Windows.UI.Xaml.Application.IApplicationInitializationCallback hiding (iid)
-- import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams hiding (iid)
import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown hiding (iid)
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.Lowlevel.Info

interfaceName :: String
interfaceName = "Windows.UI.Xaml.IApplicationOverrides"

iid :: IID IApplicationOverridesVtbl 
iid = IID $ GUID 0x25F99FF7 0x9347 0x459A 0x9F 0xAC 0xB2 0xD0 0xE1 0x1C 0x1A 0x0F

data IApplicationOverridesVtbl = IApplicationOverridesVtbl {
   inspectable :: IInspectableVtbl,
   cfp_OnActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnLaunched :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnFileActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnSearchActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnShareTargetActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnFileOpenPickerActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnFileSavePickerActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnCachedFileUpdaterActivated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT),
   cfp_OnWindowCreated :: FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT)
}

foreign import ccall "wrapper"
      mkDelegate :: (Ptr IApplicationOverrides → Ptr () → IO HRESULT) → IO (FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT))

foreign import ccall "dynamic"
   fromDelegate ::  
      FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT) 
      → (Ptr IApplicationOverrides → Ptr () → IO HRESULT)

inspectableSize :: Int
inspectableSize = sizeOf (undefined :: IInspectableVtbl)

instance Storable IApplicationOverridesVtbl where
   sizeOf _ = inspectableSize + 9 * sizeOfMethodPtr
   alignment _ = alignment (undefined :: IInspectableVtbl)
   peekByteOff ptr off = do
      inspectable ← peekByteOff ptr off
      cfp_OnActivated ← peekByteOff ptr (0 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnLaunched ← peekByteOff ptr (1 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnFileActivated ← peekByteOff ptr (2 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnSearchActivated ← peekByteOff ptr (3 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnShareTargetActivated ← peekByteOff ptr (4 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnFileOpenPickerActivated ← peekByteOff ptr (5 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnFileSavePickerActivated ← peekByteOff ptr (6 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnCachedFileUpdaterActivated ← peekByteOff ptr (7 * sizeOfMethodPtr + inspectableSize + off)
      cfp_OnActivated ← peekByteOff ptr (8 * sizeOfMethodPtr + inspectableSize + off)
      return IApplicationOverridesVtbl {..}
   poke ptr IApplicationOverridesVtbl{..} = do
      pokeByteOff ptr 0 inspectable
      pokeByteOff ptr (0 * sizeOfMethodPtr + inspectableSize) cfp_OnActivated
      pokeByteOff ptr (1 * sizeOfMethodPtr + inspectableSize) cfp_OnLaunched
      pokeByteOff ptr (2 * sizeOfMethodPtr + inspectableSize) cfp_OnFileActivated
      pokeByteOff ptr (3 * sizeOfMethodPtr + inspectableSize) cfp_OnSearchActivated
      pokeByteOff ptr (4 * sizeOfMethodPtr + inspectableSize) cfp_OnShareTargetActivated
      pokeByteOff ptr (5 * sizeOfMethodPtr + inspectableSize) cfp_OnFileOpenPickerActivated
      pokeByteOff ptr (6 * sizeOfMethodPtr + inspectableSize) cfp_OnFileSavePickerActivated
      pokeByteOff ptr (7 * sizeOfMethodPtr + inspectableSize) cfp_OnCachedFileUpdaterActivated
      pokeByteOff ptr (8 * sizeOfMethodPtr + inspectableSize) cfp_OnWindowCreated

type IApplicationOverrides = Ptr IApplicationOverridesVtbl
   
type ApplicationOverrides = RuntimeInstance IApplicationOverridesVtbl