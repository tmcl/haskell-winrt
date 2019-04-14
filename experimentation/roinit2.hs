{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Numeric
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad.Except
import Debug.Trace
import GHC.Word
import GUID
import Control.Monad

data TrustLevel

data HSTRING__
type HSTRING = Ptr HSTRING__

type HRESULT = CULong

type IID = GUID
type REFIID = Ptr IID

type QueryInterfaceType = Ptr IInspectable → REFIID → Ptr (Ptr ()) → IO HRESULT
type GetIidsType = Ptr IInspectable → Ptr CULong → Ptr (Ptr GUID) → IO HRESULT
data IInspectableVtbl = IInspectableVtbl {
   c_QueryInterface :: FunPtr QueryInterfaceType,
   c_AddRef :: FunPtr (Ptr IInspectable → IO CULong),
   c_Release :: FunPtr (Ptr IInspectable → IO CULong),

   c_GetIids :: FunPtr GetIidsType,
   c_GetRuntimeClassName :: FunPtr (Ptr IInspectable → Ptr HSTRING → IO HRESULT),
   c_GetTrustLevel :: FunPtr (Ptr IInspectable → Ptr TrustLevel → IO HRESULT)
}


type IInspectable = Ptr IInspectableVtbl

foreign import ccall "RoInitialize"
    c_RoInitialize :: CUInt → IO HRESULT

foreign import ccall "dynamic"
   mkQueryInterface :: FunPtr QueryInterfaceType → QueryInterfaceType
foreign import ccall "dynamic"
   mkGetIids :: FunPtr GetIidsType → GetIidsType

sizeOfMethodPtr :: Int
sizeOfMethodPtr = 8

instance Storable IInspectableVtbl where
   sizeOf _ = 6 * sizeOfMethodPtr
   alignment _ = sizeOfMethodPtr 
   peekByteOff ptr off = do
      c_QueryInterface ← peekByteOff ptr (0 * sizeOfMethodPtr)
      c_AddRef ← peekByteOff ptr (1 * sizeOfMethodPtr)
      c_Release ← peekByteOff ptr (2 * sizeOfMethodPtr)
      c_GetIids ← peekByteOff ptr (3 * sizeOfMethodPtr)
      c_GetRuntimeClassName ← peekByteOff ptr (4 * sizeOfMethodPtr)
      c_GetTrustLevel ← peekByteOff ptr (5 * sizeOfMethodPtr)
      return IInspectableVtbl {..}
   pokeByteOff ptr off IInspectableVtbl {..} = do
      pokeByteOff ptr (0 * sizeOfMethodPtr) c_QueryInterface
      pokeByteOff ptr (1 * sizeOfMethodPtr) c_AddRef
      pokeByteOff ptr (2 * sizeOfMethodPtr) c_Release
      pokeByteOff ptr (3 * sizeOfMethodPtr) c_GetIids
      pokeByteOff ptr (4 * sizeOfMethodPtr) c_GetRuntimeClassName
      pokeByteOff ptr (5 * sizeOfMethodPtr) c_GetTrustLevel

foreign import ccall "RoActivateInstance"
    c_RoActivateInstance :: HSTRING → Ptr (Ptr IInspectable) → IO HRESULT


foreign import ccall "WindowsCreateString"
    c_WindowsCreateString :: CWString → CUInt → Ptr HSTRING → IO HRESULT

foreign import ccall "WindowsDeleteString"
    c_WindowsDeleteString :: HSTRING → IO HRESULT

ifGood :: String → IO HRESULT → IO () → IO () 
ifGood notice test good = do
   res ← test
   if res == 0
      then do
         putStrLn $ notice <> " was good"
         good
      else
         putStrLn . showHex res $ " " <> notice <> " was bad"

alloca2 ∷ (Storable a, Storable b) ⇒ (Ptr a → Ptr b → IO c) → IO c
alloca2 f = alloca $ \ p1 → alloca $ \p2 → f p1 p2

main :: IO ()
main = do
   let guid = GUID 0x74B861A1
                      0x7487
                      0x46A9
                      0x9A 0x6E 
                      0xC7 0x8B 0x51 0x27 0x26 0xC5
   alloca $ \p_guid →do
      poke p_guid guid
      ifGood "roinit" (c_RoInitialize 1) $ do
         let appClassName = "Windows.UI.Xaml.Application"
         alloca $ \p_hstring →
            withCWStringLen appClassName $ \(c_appClassName, len) → 
               ifGood "wcs" (c_WindowsCreateString c_appClassName (fromIntegral len) p_hstring) $ do
                  hstring ← peek p_hstring
                  alloca $ \(ppinspApp :: Ptr (Ptr IInspectable)) →
                     ifGood "roacti" (c_RoActivateInstance hstring ppinspApp) $ do
                        pinspApp :: Ptr IInspectable ← peek ppinspApp
                        inspApp :: IInspectable ← peek pinspApp
                        let blah :: Ptr IInspectableVtbl = inspApp
                        ifGood "delete" (c_WindowsDeleteString hstring) . alloca $ \ppApplication → do
                           myInspApp ← peek pinspApp
                           myVtable ← peek myInspApp
                           let IInspectableVtbl {..} = myVtable
                           let queryInterface = mkQueryInterface c_QueryInterface
                           ifGood "queryInterface" (queryInterface pinspApp p_guid ppApplication) . alloca2 $ \p_iidCount p_iids → do
                              let getIids = mkGetIids c_GetIids
                              ifGood "getiids" (getIids pinspApp p_iidCount p_iids) $ do
                                 iidCount ← peek p_iidCount
                                 iids ← peek p_iids
                                 print iidCount
                                 forM_ [0..fromIntegral iidCount -1] $ \i →do
                                       my_guid :: GUID ← peekElemOff iids i 
                                       print my_guid
                                       return ()
                                    