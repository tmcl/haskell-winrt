{-# LANGUAGE RecursiveDo, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Windows.UI.Xaml.Application.IApplicationOverrides_impl where

import qualified Windows.UI.Xaml.Application.IApplicationOverrides
import Windows.UI.Xaml.Application.IApplicationOverrides hiding (iid)
import Windows.UI.Xaml.Application.IApplicationInitializationCallback hiding (iid, iids)
-- import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams hiding (iid)
import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown hiding (iid)
import qualified System.Windows.WinRT.IUnknown
import System.Windows.WinRT.Monad
import Data.Maybe

data IApplicationOverridesDetail = IApplicationOverridesDetail {
   m_OnActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnLaunched :: Maybe (Ptr () → IO HRESULT),
   m_OnFileActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnSearchActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnShareTargetActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnFileOpenPickerActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnFileSavePickerActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnCachedFileUpdaterActivated :: Maybe (Ptr () → IO HRESULT),
   m_OnWindowCreated :: Maybe (Ptr () → IO HRESULT)
}

def :: IApplicationOverridesDetail
def = IApplicationOverridesDetail {
   m_OnActivated = Nothing,
   m_OnLaunched = Nothing,
   m_OnFileActivated = Nothing,
   m_OnSearchActivated = Nothing,
   m_OnShareTargetActivated = Nothing,
   m_OnFileOpenPickerActivated = Nothing,
   m_OnFileSavePickerActivated = Nothing,
   m_OnCachedFileUpdaterActivated = Nothing,
   m_OnWindowCreated = Nothing
}
data IApplicationOverridesConcrete = IApplicationOverridesConcrete {
   onActivated :: Ptr () → IO HRESULT,
   onLaunched :: Ptr () → IO HRESULT,
   onFileActivated :: Ptr () → IO HRESULT,
   onSearchActivated :: Ptr () → IO HRESULT,
   onShareTargetActivated :: Ptr () → IO HRESULT,
   onFileOpenPickerActivated :: Ptr () → IO HRESULT,
   onFileSavePickerActivated :: Ptr () → IO HRESULT,
   onCachedFileUpdaterActivated :: Ptr () → IO HRESULT,
   onWindowCreated :: Ptr () → IO HRESULT
}

extend :: ApplicationOverrides → IApplicationOverridesDetail → IApplicationOverridesConcrete
extend base overrides = IApplicationOverridesConcrete {
   onActivated = fromMaybe (c_OnActivated base) (m_OnActivated overrides),
   onLaunched = fromMaybe (c_OnLaunched base) (m_OnLaunched overrides),
   onFileActivated = fromMaybe (c_OnFileActivated base) (m_OnFileActivated overrides),
   onSearchActivated = fromMaybe (c_OnSearchActivated base) (m_OnSearchActivated overrides),
   onShareTargetActivated = fromMaybe (c_OnShareTargetActivated base) (m_OnShareTargetActivated overrides),
   onFileOpenPickerActivated = fromMaybe (c_OnFileOpenPickerActivated base) (m_OnFileOpenPickerActivated overrides),
   onFileSavePickerActivated = fromMaybe (c_OnFileSavePickerActivated base) (m_OnFileSavePickerActivated overrides),
   onCachedFileUpdaterActivated = fromMaybe (c_OnCachedFileUpdaterActivated base) (m_OnCachedFileUpdaterActivated overrides),
   onWindowCreated = fromMaybe (c_OnWindowCreated base) (m_OnWindowCreated overrides)
}

iids :: [GUID]
iids = 
   [ uniid System.Windows.WinRT.IUnknown.iid
   , uniid System.Windows.WinRT.Inspectable.iid
   , uniid Windows.UI.Xaml.Application.IApplicationOverrides.iid ]

type ApplicationOverridesDetails = IUnknownImpl IApplicationOverridesConcrete

toDelegate'  :: (IApplicationOverridesConcrete → Ptr () → IO HRESULT)
   → Ptr (ApplicationOverridesDetails)
   → Ptr () → IO HRESULT
toDelegate' delegate p_it p_info  = do
   IUnknownImpl {..} ← peek p_it
   IUnknownImplData {..} ← deRefStablePtr impl_stbl
   delegate impld_localInfo p_info

toDelegate :: (IApplicationOverridesConcrete → Ptr () → IO HRESULT)
   → IO (FunPtr (Ptr IApplicationOverrides → Ptr () → IO HRESULT))
toDelegate = fmap castFunPtr . mkDelegate . toDelegate' 

foreign import ccall "wrapper" mkDelegate 
      :: (Ptr (IUnknownImpl IApplicationOverridesConcrete) → Ptr () → IO HRESULT) 
      → IO (FunPtr (Ptr (IUnknownImpl IApplicationOverridesConcrete) → Ptr () → IO HRESULT))

newtype ApplicationOverridesImpl = ApplicationOverridesImpl IApplicationOverridesConcrete
instance HasVtbl IApplicationOverridesConcrete where
   newtype VTable IApplicationOverridesConcrete = ApplicationOverridesVtblImpl IApplicationOverridesVtbl
      deriving (Storable)
   freeFunPtrs (ApplicationOverridesVtblImpl (IApplicationOverridesVtbl a b c d e f g h i j)) = do
      freeInspectableFunPtrs a
      freeHaskellFunPtr b
      freeHaskellFunPtr c
      freeHaskellFunPtr d
      freeHaskellFunPtr e
      freeHaskellFunPtr f
      freeHaskellFunPtr g
      freeHaskellFunPtr h
      freeHaskellFunPtr i
      freeHaskellFunPtr j
   mkVtbl addRef release = do
      qi ← mkDefaultQueryInterface iids
      let iunknown = iUnknownVtbl qi addRef release
      inspectable ← mkIInspectableVtbl iunknown iids "RandomClass.App" BaseTrust

      cfp_OnActivated ←toDelegate onActivated
      cfp_OnLaunched ← toDelegate onLaunched
      cfp_OnFileActivated ← toDelegate onFileActivated
      cfp_OnSearchActivated ← toDelegate onSearchActivated
      cfp_OnShareTargetActivated ← toDelegate onShareTargetActivated
      cfp_OnFileOpenPickerActivated ← toDelegate onFileOpenPickerActivated
      cfp_OnFileSavePickerActivated ← toDelegate onFileSavePickerActivated
      cfp_OnCachedFileUpdaterActivated ← toDelegate onCachedFileUpdaterActivated
      cfp_OnWindowCreated ← toDelegate onWindowCreated

      return $ ApplicationOverridesVtblImpl IApplicationOverridesVtbl {..}
