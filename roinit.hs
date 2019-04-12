{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Numeric
import Foreign.C hiding (CWString)
import Foreign.Ptr
import Foreign.ForeignPtr
--import qualified Foreign.Concurrent as C
--import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Foreign
import Data.Text.Internal 
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Monad.Except
import Debug.Trace
import GHC.Word

type HRESULT = CUInt
type CWString = Ptr GHC.Word.Word16 -- on windows, wchar_t ~= unsigned short

foreign import ccall "RoInitialize"
    c_RoInitialize :: CUInt → IO HRESULT

foreign import ccall "WindowsCreateString"
    c_WindowsCreateString :: CWString → CUInt → Ptr HSTRING' → IO HRESULT

foreign import ccall "WindowsDeleteString"
    c_WindowsDeleteString :: HSTRING' → IO HRESULT

foreign import ccall "&WindowsDeleteString"
    cfp_WindowsDeleteString :: FunPtr (HSTRING' → IO ())

foreign import ccall "RoActivateInstance"
    c_RoActivateInstance :: HSTRING' → Ptr Inspectable' → IO HRESULT

-- a guid is a uuid, except that it 
-- is little endian (altho it includes
-- a byte array, so parts of it look BE).
-- therefore don't unguid it; instead
-- use toUUID
data GUID = GUID
 !Word32
 !Word16
 !Word16
 !Word8 !Word8
 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8


instance Storable GUID where
    sizeOf _ = 16
    alignment _ = 4

    peekByteOff p off =
       GUID
             <$> peekByteOff p off -- Word32
             <*> peekByteOff p (off+4) -- Word16
             <*> peekByteOff p (off+6) -- Word16
             <*> peekByteOff p (off+8) -- Word8
             <*> peekByteOff p (off+9) -- Word8
             <*> peekByteOff p (off+10) -- Word8
             <*> peekByteOff p (off+11) -- Word8
             <*> peekByteOff p (off+12) -- Word8
             <*> peekByteOff p (off+13) -- Word8
             <*> peekByteOff p (off+14) -- Word8
             <*> peekByteOff p (off+15) -- Word8
        

    pokeByteOff p off 
          (GUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = 
              do
                pokeByteOff p off x0
                pokeByteOff p (off+4) x1
                pokeByteOff p (off+6) x2
                pokeByteOff p (off+8) x3
                pokeByteOff p (off+9) x4
                pokeByteOff p (off+10) x5
                pokeByteOff p (off+11) x6
                pokeByteOff p (off+12) x7
                pokeByteOff p (off+13) x8
                pokeByteOff p (off+14) x9
                pokeByteOff p (off+15) x10


class HasIID a where
  getIid ∷ IID (Ptr a)

-- actually, this is a pointer to a bunch of function pointers
type IInspectable = Ptr (Ptr ())
type Inspectable' = Ptr IInspectable
type Inspectable = ForeignPtr IInspectable

data WinRtClass a = WinRtClass GUID a

type QueryInterfaceType b = Inspectable' → Ptr (IID b) → Ptr b → IO HRESULT
foreign import ccall "dynamic"
   mkQIT :: FunPtr (QueryInterfaceType b) → QueryInterfaceType b

data IApplication
type Application' = Ptr IApplication
type Application = ForeignPtr IApplication
instance HasIID IApplication where
  getIid = IID $ GUID 0x74B861A1
                      0x7487
                      0x46A9
                      0x9A 0x6E 
                      0xC7 0x8B 0x51 0x27 0x26 0xC5

newtype CLSID = CLSID GUID 
newtype IID a = IID GUID -- phantom a
   deriving (Storable)

alloca2 ∷ (MonadIO m, Storable a, Storable b) ⇒ (Ptr a → Ptr b → IO c) → m c
alloca2 f = liftIO $ alloca $ \ p1 → liftIO $ alloca $ \p2 → f p1 p2

-- queryInterface ∷ HasIID b ⇒ Inspectable → WinRtStep (ForeignPtr b)
queryInterface ∷ Inspectable → WinRtStep Application
queryInterface insp = do
   let iid ∷ IID Application' = getIid 
   out ← (throwHResult =<<) . alloca2 $ \ p_iid p_out → do
      poke p_iid iid
      withForeignPtr insp $ \p_insp → do
         struct ← peek p_insp
         vtbl ← peek struct
         let 
            q = mkQIT $ (castPtrToFunPtr ∷ Ptr () → FunPtr b) vtbl
         hres ← q p_insp p_iid p_out
         out ← peek p_out
         return (hres, out)
   liftIO $ newForeignPtr_  out
 

      
-- | To adequately understand HSTRINGs, consider reading
--    https://devblogs.microsoft.com/oldnewthing/20160615-00/?p=93675
--    "Raymond's complete guide to HSTRING semantics.
-- | HSTRING__ is opaque/abstract,  even in C++
data HSTRING__
-- | A HSTRING is a handle (pointer) to a HSTRING__
newtype HSTRING = HSTRING { unhstring ∷ ForeignPtr HSTRING__ }
-- | there are strong restrictions on what we can do with
-- a hstring that has been passed to us, since it might be 
-- a stack allocated fast-pass HSTRING
newtype HSTRINGin = HSTRINGin HSTRING'
type HSTRING' = Ptr HSTRING__
-- | At the moment I've only implemented mallocy refcounted 
-- HSTRINGs, since they fit Haskell's semantics better. 
-- There's also fastpass, stack-allocated HSTRINGs. Maybe
-- one day.

-- naming convention:
--    * something begining with "h" is a handle to a thing

withForeignPtrs ∷ ForeignPtr a → ForeignPtr b → (Ptr a → Ptr b → IO c) → IO c
withForeignPtrs fp_a fp_b run = 
   withForeignPtr fp_a $ \p_a → 
      withForeignPtr fp_b $ \p_b → 
         run p_a p_b

main :: IO ()
main = do
   putStrLn "beginning"
   i <- c_RoInitialize 1
   print i

   let appClassName' = "Windows.UI.Xaml.Application"
   _ ← runExceptT $ do 
      appClassName ← textToHSTRING appClassName'
      insp ← activateInstance appClassName
      _ ∷ Application ← queryInterface insp
      return ()
   
   putStrLn "done"

throwHResult ∷ (Monad m) ⇒ (HRESULT, a) → ExceptT HRESULT m a
throwHResult (hres, a) 
   | hres >= 0x8000_0000 = trace (showHex hres " error") $ throwError hres
   | otherwise = pure a

textToHSTRING ∷ Text → WinRtStep HSTRING
textToHSTRING t = do
   -- fpHstr is a pointer to a pointer
   hstr' ← (throwHResult =<<) . liftIO $ do
      alloca $ \pHstr → do
         useAsPtr t $ \pTextBuf len → do
            hres ← c_WindowsCreateString pTextBuf (fromIntegral len)  pHstr
            hstr' ← peek pHstr
            return (hres, hstr')
   -- okay, so if we're here nothing went wrong
   -- lets attach a finaliser and wrap it up
   hstr'fp ← liftIO $ do
      --cfp_WindowsDeleteString ← mkWindowsDeleteString 
      --   c_WindowsDeleteString
      newForeignPtr cfp_WindowsDeleteString hstr'
   pure $ HSTRING hstr'fp

foreign import ccall "wrapper"
   mkWindowsDeleteString :: (HSTRING' → IO ()) 
                          → IO (FunPtr (HSTRING' → IO ()))


activateInstance ∷ HSTRING → WinRtStep Inspectable
activateInstance (HSTRING fp_appClassName) = do
   inspApp ← (throwHResult =<<) . liftIO $ do
      withForeignPtr fp_appClassName $ \p_appClassName → do
         alloca $ \p_inspApp → do
            hres ← c_RoActivateInstance p_appClassName p_inspApp
            inspApp ← peek p_inspApp
            return (hres, inspApp)
   -- TODO: release the inspapp
   liftIO $ newForeignPtr_ inspApp
   
type WinRtStep = ExceptT HRESULT IO

mkWchar_t ∷ Text → IO (ForeignPtr CWchar, CUInt)
mkWchar_t t@(Text _arr _off len) = do
   fp ← mallocForeignPtrArray (len + 1)
   withForeignPtr fp $ \ptr → do
      unsafeCopyToPtr t ptr
      pokeElemOff ptr len 0
   return (castForeignPtr fp, fromIntegral len)
   
