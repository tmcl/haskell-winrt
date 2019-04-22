{-# LANGUAGE RecursiveDo #-}
module Windows.UI.Xaml.Application.IApplicationOverrides_impl where

import Windows.UI.Xaml.Application hiding (classId, iid)
import qualified Windows.UI.Xaml.Application 
import Windows.UI.Xaml.Application.IApplicationInitializationCallback hiding (iid)
-- import Windows.UI.Xaml.Application.IApplicationInitializationCallbackParams hiding (iid)
import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Inspectable
import System.Windows.WinRT.IUnknown hiding (iid)
import System.Windows.WinRT.Monad
import System.Windows.WinRT.RoInit
import System.Windows.WinRT.Lowlevel.Info

data IApplicationOverridesImpl = IApplicationOverridesImpl {
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

makeConcrete :: IApplicationOverridesImpl → ApplicationOverrides → IO ApplicationOverrides 
makeConcrete partialExtension base = do
   cfp_OnActivated' ← mkDelegate $ case m_OnActivated partialExtension of
      Just f → f
      Nothing → return $ cfp_OnActivated 
   where
      _ `unlessDefined` (Just g) = g
      f `unlessDefined` Nothing = f base


extend :: IApplicationOverridesImpl → IO IApplication
extend it = mdo
   factory :: ApplicationFactory ← getActivationFactory Windows.UI.Xaml.Application.classId
   (inner, app) ← createInstance factory (makeConcrete it inner)
   return app