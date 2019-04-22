{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.Windows.WinRT.IUnknown
where

import Foreign hiding (new)
import Foreign.C
import System.Windows.GUID
import System.Windows.WinRT.Monad
import System.Windows.WinRT.Lowlevel.Info
import Data.IORef
import Control.Monad

-- interfaceName :: String
-- interfaceName = "Windows.UI.Xaml.IApplicationInitializationCallback"

type InstanceOf menu = Ptr menu
data RuntimeInstance menu = RuntimeInstance {
    foreignPtr :: ForeignPtr (InstanceOf menu),
    vtable :: menu
}

-- withIInspectable :: Inspectable → (Ptr IInspectable → WinRT a) → WinRT a
-- withIInspectable Inspectable{..} act = passthrough . withForeignPtr insp_foreignPtr $ act

withRuntimeInstance :: RuntimeInstance menu → (menu → Ptr (InstanceOf menu) → WinRT a) → WinRT a
withRuntimeInstance RuntimeInstance{..} action = 
    passthrough (withForeignPtr foreignPtr) (action vtable)

iid :: IID IUnknownVtbl 
iid = IID $ GUID 0x00000000 0x0000 0x0000
   0xC0 0x00 0x00 0x00 0x00 0x00 0x00 0x46

type REFIID = Ptr GUID
type UnsafeQueryInterfaceType = Ptr IUnknown → REFIID → Ptr (Ptr IUnknown) → IO HRESULT
type QueryInterfaceType b = Ptr IUnknown → Ptr (IID b) → Ptr (Ptr (Ptr b)) → IO HRESULT

type RefCountMethod = Ptr IUnknown → IO CULong
type RefCountMethodImpl a = Ptr (IUnknownImpl a)→ IO CULong

data IUnknownVtbl = IUnknownVtbl {
   cfp_QueryInterface :: FunPtr UnsafeQueryInterfaceType,
   cfp_AddRef :: FunPtr RefCountMethod,
   cfp_Release :: FunPtr RefCountMethod
}

type IUnknown = Ptr IUnknownVtbl

type Unknown = RuntimeInstance IUnknownVtbl

iUnknownVtbl :: 
   FunPtr UnsafeQueryInterfaceType
   → FunPtr (RefCountMethodImpl a)
   → FunPtr (RefCountMethodImpl a)
   → IUnknownVtbl
iUnknownVtbl qi addRef release = IUnknownVtbl
   qi 
   (castFunPtr addRef)
   (castFunPtr release)


instance Storable IUnknownVtbl where
   sizeOf _ = 3 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 

   peekByteOff ptr off = do
      cfp_QueryInterface ← peekByteOff ptr (off + 0 * sizeOfMethodPtr)
      cfp_AddRef ← peekByteOff ptr (off + 1 * sizeOfMethodPtr)
      cfp_Release ← peekByteOff ptr (off + 2 * sizeOfMethodPtr)
      return IUnknownVtbl {..}

   pokeByteOff ptr off IUnknownVtbl {..} = do
      pokeByteOff ptr (off + 0 * sizeOfMethodPtr) cfp_QueryInterface
      pokeByteOff ptr (off + 1 * sizeOfMethodPtr) cfp_AddRef
      pokeByteOff ptr (off + 2 * sizeOfMethodPtr) cfp_Release

data HsUnknown a = MyUnknown {
   numRefs :: Word,
   something :: a
}

type MyUnknown a = IORef (HsUnknown a)

foreign import ccall "wrapper"
      mkRefCounter' :: RefCountMethodImpl a → IO (FunPtr (RefCountMethodImpl a))

mkRefCounter :: RefCountMethodImpl a → IO (FunPtr RefCountMethod)
mkRefCounter = fmap castFunPtr . mkRefCounter'

foreign import ccall "wrapper"
      mkInterfaceQuerier :: UnsafeQueryInterfaceType → IO (FunPtr UnsafeQueryInterfaceType)

mkDefaultQueryInterface :: IO (FunPtr UnsafeQueryInterfaceType) 
mkDefaultQueryInterface = mkInterfaceQuerier defaultQueryInterface

defaultQueryInterface :: UnsafeQueryInterfaceType
defaultQueryInterface p_in p_iid pp_out = do
   targetIID ← peek p_iid
   if uniid iid == targetIID 
      then do
         -- todo: increase refcount in this unlikely event
         poke pp_out p_in
         return 0
      else return e_no_interface



hc_QueryInterface :: UnsafeQueryInterfaceType
hc_QueryInterface p_in p_iid p_out = do
   guid ← peek p_iid
   res ← h_QueryInterface (castPtrToStablePtr $ castPtr p_in) (IID guid)
   case res of
      Left hres → return hres
      Right out → do
         poke p_out out
         return 0

h_QueryInterface :: Storable a => StablePtr (MyUnknown ()) → IID a → IO (Either HRESULT a)
h_QueryInterface _ guid = error $ "someone wanted to query the interface for " ++ show guid

mkIunknownvtbl :: HasVtbl a => IO (VTable a)
mkIunknownvtbl = do
   cfp_AddRef ← mkRefCounter' addRefInternal
   cfp_Release ← mkRefCounter' releaseInternal
   mkVtbl cfp_AddRef cfp_Release
   -- cfp_QueryInterface ← mkInterfaceQuerier hc_QueryInterface
   -- return IUnknownVtbl {..}

newIUnknownImpl ::  HasVtbl a => IO (IUnknownImpl a)
newIUnknownImpl = do
   iunknownvtbl ← mkIunknownvtbl 
   f_theMem :: ForeignPtr (IUnknownImpl a) ← mallocForeignPtr
   let theState = IUnknownImplState 1
   ioref ← newIORef theState
   let theData = IUnknownImplData ioref f_theMem
   theStableData ← newStablePtr theData
   let theIUnknownImpl = IUnknownImpl iunknownvtbl theStableData
   withForeignPtr f_theMem $ \p_theMem →
      poke p_theMem theIUnknownImpl
   return theIUnknownImpl

data HasVtbl a => IUnknownImplData a = IUnknownImplData {
   impld_ioref :: IORef IUnknownImplState,
   impld_fmem :: ForeignPtr (IUnknownImpl a)
}

class Storable (VTable a) => HasVtbl a where
   data VTable a 
   mkVtbl :: FunPtr (RefCountMethodImpl a) 
      → FunPtr (RefCountMethodImpl a) 
      → IO (VTable a)
   freeFunPtrs :: VTable a → IO ()
   -- sizeOf :: VTable a → Int

-- instance Storable (VTable a)

data HasVtbl a => IUnknownImpl a = IUnknownImpl {
   impl_vtbl :: VTable a,
   impl_stbl :: StablePtr (IUnknownImplData a)
}

calcPtrOffsets :: HasVtbl a => Ptr (IUnknownImpl a)→ (
   Ptr (Ptr (VTable a)), 
   Ptr (StablePtr (IUnknownImplData a)), 
   Ptr (VTable a))
calcPtrOffsets ptr = (checkPtr, storablePtr, vtblPtr)
   where 
         checkPtr = ptr `plusPtr` 0
         storablePtr = ptr `plusPtr` sizeOf checkPtr
         vtblPtr = ptr `plusPtr` (sizeOf checkPtr + sizeOf storablePtr)

instance HasVtbl a => Storable (IUnknownImpl a) where
   sizeOf it = 
      sizeOf (undefined :: Ptr (VTable a)) 
      + sizeOf (impl_vtbl it)
      + sizeOf (impl_stbl it)
   alignment _ = alignment (undefined :: Ptr ())
   peek ptr = do
      let (checkPtr, storablePtr, vtblPtr) = calcPtrOffsets ptr
      vtblPtr' ← peek checkPtr
      unless (vtblPtr' == vtblPtr) $ 
         fail "this isn't one of mine"
      impl_stbl ← peek storablePtr
      impl_vtbl ← peek vtblPtr
      return IUnknownImpl {..}
   poke ptr IUnknownImpl{..} = do
      let (checkPtr, storablePtr, vtblPtr) = calcPtrOffsets ptr
      poke checkPtr vtblPtr
      poke storablePtr impl_stbl
      poke vtblPtr impl_vtbl

addRefInternal :: HasVtbl a => Ptr (IUnknownImpl a) → IO CULong
addRefInternal p_it = do
   IUnknownImpl {..} ← peek p_it
   stableData ← deRefStablePtr impl_stbl
   res ← atomicModifyIORef (impld_ioref stableData) (\old → let new = updateRefCount old 1 in (new, new))
   return $ refsCount res

releaseInternal :: HasVtbl a => Ptr (IUnknownImpl a) → IO CULong
releaseInternal p_it = do
   IUnknownImpl {..} ← peek p_it
   stableData ← deRefStablePtr impl_stbl
   res' ←  atomicModifyIORef (impld_ioref stableData) (\old → let new = updateRefCount old (-1) in (new, new))
   let res = refsCount res'
   when (res == 0) $ do
      freeStablePtr impl_stbl
      freeFunPtrs impl_vtbl
   return res

-- freeFunPtrs :: IUnknownVtbl → IO ()
-- -- we don't use {..} here since we want to free them _all_,
-- -- even if code changes in the future to create more
-- freeFunPtrs (IUnknownVtbl cfp_a cfp_b cfp_c) = do
--    freeHaskellFunPtr cfp_a
--    freeHaskellFunPtr cfp_b
--    freeHaskellFunPtr cfp_c


newtype IUnknownImplState = IUnknownImplState {
   refsCount :: CULong
}

updateRefCount :: IUnknownImplState → CULong → IUnknownImplState
updateRefCount old diff = old { refsCount = refsCount old + diff }
