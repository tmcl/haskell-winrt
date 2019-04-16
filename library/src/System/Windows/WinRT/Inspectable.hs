{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-} -- todo remove me


module System.Windows.WinRT.Inspectable (Inspectable(), newInspectable)
where

import Foreign
import Foreign.C.Types
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.HSTRING
import System.Windows.WinRT.Lowlevel.Info
import Control.Monad.IO.Class
import Unsafe.Coerce

data TrustLevel

foreign import ccall "RoActivateInstance"
    c_RoActivateInstance :: HSTRING → Ptr (Ptr IInspectable) → IO HRESULT

data Inspectable = Inspectable { 
   insp_foreignPtr :: ForeignPtr IInspectable,
   insp_vtable :: IInspectableVtbl
}

inspectableInfo :: InspectableInfo Inspectable
inspectableInfo = InspectableInfo mclassName miid 


miid = IID (GUID 0 0 0 0 0 0 0 0 0 0 0) -- TODO fixme

mclassName = pack "IInspectable"


type IInspectable = Ptr IInspectableVtbl



type REFIID = Ptr GUID
type UnsafeQueryInterfaceType = Ptr IInspectable → REFIID → Ptr (Ptr ()) → IO HRESULT
type QueryInterfaceType b = Ptr IInspectable → Ptr (IID b) → Ptr (Ptr (Ptr b)) → IO HRESULT
type GetIidsType = Ptr IInspectable → Ptr CULong → Ptr (Ptr GUID) → IO HRESULT
data IInspectableVtbl = IInspectableVtbl {
   cfp_QueryInterface :: FunPtr UnsafeQueryInterfaceType,
   cfp_AddRef :: FunPtr (Ptr IInspectable → IO CULong),
   cfp_Release :: FunPtr (Ptr IInspectable → IO ()), -- actually a HRESULT, but I need to run it in a finaliser

   cfp_GetIids :: FunPtr GetIidsType,
   cfp_GetRuntimeClassName :: FunPtr (Ptr IInspectable → Ptr HSTRING → IO HRESULT),
   cfp_GetTrustLevel :: FunPtr (Ptr IInspectable → Ptr TrustLevel → IO HRESULT)
}

instance Storable IInspectableVtbl where
   sizeOf _ = 6 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 
   peekByteOff ptr off = do
      cfp_QueryInterface ← peekByteOff ptr (0 * sizeOfMethodPtr)
      cfp_AddRef ← peekByteOff ptr (1 * sizeOfMethodPtr)
      cfp_Release ← peekByteOff ptr (2 * sizeOfMethodPtr)
      cfp_GetIids ← peekByteOff ptr (3 * sizeOfMethodPtr)
      cfp_GetRuntimeClassName ← peekByteOff ptr (4 * sizeOfMethodPtr)
      cfp_GetTrustLevel ← peekByteOff ptr (5 * sizeOfMethodPtr)
      return IInspectableVtbl {..}
   pokeByteOff ptr off IInspectableVtbl {..} = do

      pokeByteOff ptr (0 * sizeOfMethodPtr) cfp_QueryInterface
      pokeByteOff ptr (1 * sizeOfMethodPtr) cfp_AddRef
      pokeByteOff ptr (2 * sizeOfMethodPtr) cfp_Release
      pokeByteOff ptr (3 * sizeOfMethodPtr) cfp_GetIids
      pokeByteOff ptr (4 * sizeOfMethodPtr) cfp_GetRuntimeClassName
      pokeByteOff ptr (5 * sizeOfMethodPtr) cfp_GetTrustLevel

foreign import ccall "dynamic"
   mkQueryInterface' :: FunPtr UnsafeQueryInterfaceType → UnsafeQueryInterfaceType

newtype QIT a = QIT { unQIT :: QueryInterfaceType a }
mkQueryInterface :: FunPtr UnsafeQueryInterfaceType → QIT a
mkQueryInterface fp = QIT (unsafeCoerce (mkQueryInterface' fp))

foreign import ccall "dynamic"
   mkGetIids :: FunPtr GetIidsType → GetIidsType

newInspectable :: HString → WinRT Inspectable
newInspectable = flip withHString $ \h_className → 
   wrap . alloca $ \(pp_inspApp :: Ptr (Ptr IInspectable)) → unwrap $ do
      try $ c_RoActivateInstance h_className pp_inspApp
      liftIO $ do
         p_inspApp ← peek pp_inspApp
         inspApp ← peek p_inspApp
         insp_vtable ← peek inspApp
         let IInspectableVtbl {..} = insp_vtable
         insp_foreignPtr ← newForeignPtr_ p_inspApp
         -- insp_foreignPtr ← newForeignPtr c_Release p_inspApp
         return Inspectable {..}


-- class IsUnknown a where
--    type VTable a :: *
--    -- iid :: a → IID (VTable a)

-- type IUnknown a = Ptr (VTable a)

-- class Storable a => VTable_ a where
--    data Instance
--    getIid :: a → IID a
--    getRuntimeClassName :: a → HString
--    make :: ForeignPtr (Ptr a) → a → Instance
   


withInspectable :: Inspectable → (Ptr IInspectable → WinRT a) → WinRT a
withInspectable Inspectable{..} = passthrough $ withForeignPtr insp_foreignPtr 

data InspectableInfo a = InspectableInfo {
   className :: HString,
   iid :: IID a
}


queryInterface :: (ForeignPtr IInspectable → IInspectableVtbl →Inspectable) → IID IInspectableVtbl → Inspectable → WinRT Inspectable
queryInterface make my_guid inspectable =
   -- let my_guid = iid (undefined :: a)
   withInspectable inspectable $ \p_inspApp →
      alloca2' $ \p_guid (pp_res :: Ptr (Ptr (Ptr IInspectableVtbl))) → do
         liftIO $ poke p_guid my_guid
         let 
            Inspectable {..} = inspectable 
            IInspectableVtbl {..} = insp_vtable
            doQueryInterface :: QueryInterfaceType IInspectableVtbl
            doQueryInterface = unQIT $ mkQueryInterface cfp_QueryInterface
         try $ doQueryInterface p_inspApp p_guid pp_res
         liftIO $ do
            p_res ← peek pp_res
            res ← peek p_res
            vtbl ← peek res
            fp_res ← newForeignPtr_ p_res
            return $ make fp_res vtbl

-- new :: (IsInspectable a) => InspectableInfo a → WinRT a
-- new runtimeClass = do
--    let my_className = className runtimeClass
--    insp ← newInspectable my_className
--    out :: a ← queryInterface (iid runtimeClass) insp
--    return out
