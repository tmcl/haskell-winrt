{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}

module System.Windows.WinRT.Inspectable.QueryInterface (queryInterface, IsQueriable(..)) where


import Foreign
import Foreign.C.Types
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Inspectable.TheInterface
import System.Windows.WinRT.Lowlevel.Info
import System.Windows.WinRT.IUnknown
import Control.Monad.IO.Class
import Unsafe.Coerce

foreign import ccall "dynamic"
   mkQueryInterface' :: FunPtr UnsafeQueryInterfaceType → UnsafeQueryInterfaceType

newtype QIT a b = QIT { unQIT :: QueryInterfaceType a b }
mkQueryInterface :: FunPtr UnsafeQueryInterfaceType → QIT a b
mkQueryInterface fp = QIT (unsafeCoerce (mkQueryInterface' fp))

class (Storable a, IsUnknown a) => IsQueriable a where
   make :: ForeignPtr (Ptr a) → a → RuntimeInstance a

queryInterface :: (IsUnknown b, IsQueriable a) => IID a → (RuntimeInstance b) → WinRT (RuntimeInstance a)
queryInterface my_guid unknown =
   withRuntimeInstance unknown $ \vtbl_in p_unknown  →
      alloca2wrt $ \p_guid (pp_res :: Ptr (Ptr (Ptr a))) → do
         liftIO $ poke p_guid my_guid
         let 
            doQueryInterface :: QueryInterfaceType b a
            doQueryInterface = unQIT $ mkQueryInterface $ fp_QueryInterface vtbl_in
         try $ doQueryInterface p_unknown p_guid pp_res
         liftIO $ do
            p_res ← peek pp_res
            res ← peek p_res
            vtbl ← peek res
            fp_res ← newForeignPtr_ p_res
            return $ make fp_res vtbl