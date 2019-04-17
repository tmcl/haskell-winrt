{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}

module System.Windows.WinRT.Inspectable.QueryInterface (queryInterface) where


import Foreign
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Inspectable.TheInterface
import System.Windows.WinRT.Lowlevel.Info
import System.Windows.WinRT.IUnknown
import Control.Monad.IO.Class
import Unsafe.Coerce

foreign import ccall "dynamic"
   mkQueryInterface' :: FunPtr UnsafeQueryInterfaceType → UnsafeQueryInterfaceType

newtype QIT a = QIT { unQIT :: QueryInterfaceType a }
mkQueryInterface :: FunPtr UnsafeQueryInterfaceType → QIT a
mkQueryInterface fp = QIT (unsafeCoerce (mkQueryInterface' fp))

queryInterface :: (ForeignPtr IInspectable → IInspectableVtbl →Inspectable) → IID IInspectableVtbl → Unknown → WinRT Inspectable
queryInterface make my_guid unknown =
   withRuntimeInstance unknown $ \IUnknownVtbl{..} p_unknown  →
      alloca2' $ \p_guid (pp_res :: Ptr (Ptr (Ptr IInspectableVtbl))) → do
         liftIO $ poke p_guid my_guid
         let 
            doQueryInterface :: QueryInterfaceType IInspectableVtbl
            doQueryInterface = unQIT $ mkQueryInterface cfp_QueryInterface
         try $ doQueryInterface p_unknown p_guid pp_res
         liftIO $ do
            p_res ← peek pp_res
            res ← peek p_res
            vtbl ← peek res
            fp_res ← newForeignPtr_ p_res
            return $ make fp_res vtbl