{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}

module System.Windows.WinRT.Inspectable.TheInterface (
   Inspectable(..), 
   IInspectable, 
   IInspectableVtbl(..), 
   QueryInterfaceType,
   UnsafeQueryInterfaceType,
   IidsGetter,
   TrustLevel (..),
   mkIInspectableVtbl,
   freeInspectableFunPtrs
   )
where

import Foreign
import Foreign.C.Types
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.IUnknown
import System.Windows.WinRT.Lowlevel.HSTRING
import System.Windows.WinRT.Lowlevel.Info
import Data.Int
import Control.Monad (forM_)
import Control.Monad.IO.Class

data TrustLevel = BaseTrust
trustLevelToEnum :: TrustLevel → Int32
trustLevelToEnum BaseTrust = 0

data Inspectable = Inspectable { 
   insp_foreignPtr :: ForeignPtr IInspectable,
   insp_vtable :: IInspectableVtbl
}

type IInspectable = Ptr IInspectableVtbl

foreign import ccall "CoTaskMemAlloc"
   coTaskMemAlloc :: CSize → IO (Ptr a)

foreign import ccall "wrapper"
      mkGetIids :: IidsGetter → IO (FunPtr IidsGetter)
foreign import ccall "wrapper"
      mkGetRuntimeClassName :: RuntimeClassNameGetter → IO (FunPtr RuntimeClassNameGetter)
foreign import ccall "wrapper"
      mkGetTrustLevel :: TrustLevelGetter → IO (FunPtr TrustLevelGetter)

mkIInspectableVtbl :: IUnknownVtbl → [GUID] → String → TrustLevel → IO IInspectableVtbl
mkIInspectableVtbl iunknown guids runtimeClassName trustLevel = do
   let 
      getGuids :: IidsGetter
      getGuids _ p_num pp_guids = do
         let numGuids = length guids
         p_guids ← coTaskMemAlloc (fromIntegral $ sizeOf (undefined :: GUID) * numGuids)
         if p_guids == nullPtr
            then return e_outofmemory
         else do
            forM_ (zip [0..] guids) $ uncurry (pokeElemOff p_guids)
            poke pp_guids p_guids
            poke p_num (fromIntegral numGuids)
            return 0
      getRuntimeClassName :: RuntimeClassNameGetter
      getRuntimeClassName _ p_hstring = do
         let hstrClassName = pack runtimeClassName
         returnHresult $ withHString hstrClassName $ \hstring → 
            liftIO (poke p_hstring hstring)

      getTrustLevel :: TrustLevelGetter
      getTrustLevel _ p_trustlevel =
         poke (castPtr p_trustlevel) (trustLevelToEnum trustLevel) >> return 0

   cfp_GetIids ← mkGetIids getGuids
   cfp_GetRuntimeClassName ← mkGetRuntimeClassName getRuntimeClassName
   cfp_GetTrustLevel ← mkGetTrustLevel getTrustLevel

   return IInspectableVtbl {..}



type IidsGetter = Ptr IInspectable → Ptr CULong → Ptr (Ptr GUID) → IO HRESULT
type RuntimeClassNameGetter = Ptr IInspectable → Ptr HSTRING → IO HRESULT
type TrustLevelGetter = Ptr IInspectable → Ptr TrustLevel → IO HRESULT

data IInspectableVtbl = IInspectableVtbl {
   iunknown :: IUnknownVtbl,

   cfp_GetIids :: FunPtr IidsGetter,
   cfp_GetRuntimeClassName :: FunPtr RuntimeClassNameGetter,
   cfp_GetTrustLevel :: FunPtr TrustLevelGetter
}

instance IsUnknown IInspectableVtbl where
   fp_QueryInterface = fp_QueryInterface . iunknown


instance Storable IInspectableVtbl where
   sizeOf _ = sizeOf (undefined :: IUnknownVtbl) + 3 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 
   peekByteOff ptr off = do
      iunknown ← peekByteOff ptr off
      cfp_GetIids ← peekByteOff ptr (off + 3 * sizeOfMethodPtr)
      cfp_GetRuntimeClassName ← peekByteOff ptr (off + 4 * sizeOfMethodPtr)
      cfp_GetTrustLevel ← peekByteOff ptr (off + 5 * sizeOfMethodPtr)
      return IInspectableVtbl {..}

   pokeByteOff ptr off IInspectableVtbl {..} = do
      pokeByteOff ptr off iunknown
      pokeByteOff ptr (off + 3 * sizeOfMethodPtr) cfp_GetIids
      pokeByteOff ptr (off + 4 * sizeOfMethodPtr) cfp_GetRuntimeClassName
      pokeByteOff ptr (off + 5 * sizeOfMethodPtr) cfp_GetTrustLevel

-- withIInspectable :: Inspectable → (Ptr IInspectable → WinRT a) → WinRT a
-- withIInspectable Inspectable{..} = passthrough $ withForeignPtr insp_foreignPtr 

freeInspectableFunPtrs :: IInspectableVtbl → IO ()
freeInspectableFunPtrs (IInspectableVtbl a b c d) = do
   freeFunPtrs (IUnknownVtblImpl a)
   freeHaskellFunPtr b
   freeHaskellFunPtr c
   freeHaskellFunPtr d