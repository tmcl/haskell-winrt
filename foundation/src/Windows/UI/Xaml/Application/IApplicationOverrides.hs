{-# language ScopedTypeVariables #-}
module Windows.UI.Xaml.Application.IApplicationOverrides where

import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown hiding (iid)
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.Info

interfaceName :: String
interfaceName = "Windows.UI.Xaml.IApplicationOverrides"

iid :: IID IApplicationOverridesVtbl 
iid = IID $ GUID 0x25F99FF7 0x9347 0x459A 0x9F 0xAC 0xB2 0xD0 0xE1 0x1C 0x1A 0x0F

data IApplicationOverridesVtbl = IApplicationOverridesVtbl {
   inspectable :: IInspectableVtbl,
   cfp_OnActivated :: FunPtr Delegate,
   cfp_OnLaunched :: FunPtr Delegate,
   cfp_OnFileActivated :: FunPtr Delegate,
   cfp_OnSearchActivated :: FunPtr Delegate,
   cfp_OnShareTargetActivated :: FunPtr Delegate,
   cfp_OnFileOpenPickerActivated :: FunPtr Delegate,
   cfp_OnFileSavePickerActivated :: FunPtr Delegate,
   cfp_OnCachedFileUpdaterActivated :: FunPtr Delegate,
   cfp_OnWindowCreated :: FunPtr Delegate
}

instance IsQueriable IApplicationOverridesVtbl where
   make = RuntimeInstance

instance IsUnknown IApplicationOverridesVtbl where
   fp_QueryInterface = fp_QueryInterface . inspectable

type Delegate = Ptr IApplicationOverrides → Ptr () → IO HRESULT

callDelegate :: (IApplicationOverridesVtbl → FunPtr Delegate) 
   → ApplicationOverrides → Ptr () → IO HRESULT
callDelegate delegate overrides ptr =  do
   returnHresult $ withRuntimeInstance overrides $ \vtbl p_overrides →
      try $ fromDelegate (delegate vtbl) p_overrides ptr

flatten :: Either a a → a
flatten (Left a) = a
flatten (Right a) = a

c_OnActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnActivated = callDelegate cfp_OnActivated
      
c_OnLaunched :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnLaunched = callDelegate cfp_OnLaunched
      
c_OnFileActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnFileActivated = callDelegate cfp_OnFileActivated
      
c_OnSearchActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnSearchActivated = callDelegate cfp_OnSearchActivated
      
c_OnShareTargetActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnShareTargetActivated = callDelegate cfp_OnShareTargetActivated
      
c_OnFileOpenPickerActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnFileOpenPickerActivated = callDelegate cfp_OnFileOpenPickerActivated
      
c_OnFileSavePickerActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnFileSavePickerActivated = callDelegate cfp_OnFileSavePickerActivated
      
c_OnCachedFileUpdaterActivated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnCachedFileUpdaterActivated = callDelegate cfp_OnCachedFileUpdaterActivated
      
c_OnWindowCreated :: ApplicationOverrides → Ptr () → IO HRESULT
c_OnWindowCreated = callDelegate cfp_OnWindowCreated

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
      cfp_OnWindowCreated ← peekByteOff ptr (8 * sizeOfMethodPtr + inspectableSize + off)
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