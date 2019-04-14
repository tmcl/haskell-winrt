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
import Foreign.C
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
-- type CWString = Ptr GHC.Word.Word16 -- on windows, wchar_t ~= unsigned short

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



class HasIID a where
  getIid ∷ IID (Ptr a)

-- actually, this is a pointer to a bunch of function pointers
type IInspectable = Ptr (Ptr (FunPtr (CInt → CInt)))
type Inspectable' = Ptr IInspectable
type Inspectable = ForeignPtr IInspectable

data WinRtClass a = WinRtClass GUID a

type QueryInterfaceType b = Inspectable' → Ptr (IID b) → Ptr b → IO HRESULT
foreign import ccall "dynamic"
   mkQIT :: FunPtr (QueryInterfaceType b) → QueryInterfaceType b

type GetIIds = Inspectable' → Ptr CUInt → Ptr GUID → IO HRESULT
foreign import ccall "dynamic"
   mkGetIIds :: FunPtr GetIIds → GetIIds

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
         vtbl :: Ptr (FunPtr (CInt → CInt)) ← peek struct
         -- alloca2 $ \p_iid_len p_iids → do
         --    let n = sizeOf vtbl
         --    let getIids = mkGetIIds $ (castPtrToFunPtr ∷ Ptr (FunPtr b) → FunPtr b) (vtbl `plusPtr` (3*n) )
         --    putStrLn "asking the iids"
         --    hres ← getIids p_insp p_iid_len p_iids
         --    print $ showHex hres ""
         --    when (hres < 0x8000_0000) $ do
         --       putStrLn "describing the iids"
         --       len ← peek p_iid_len
         --       print len
         let 
            q = mkQIT $ (castPtrToFunPtr ∷ Ptr (FunPtr a) → FunPtr b) vtbl
         putStrLn "running the qit"
         hres ← q p_insp p_iid p_out
         putStrLn "ran it "
         print hres
         out ← peek p_out
         return (hres, out)
   o ← liftIO $ newForeignPtr_  out
   makeitwork o
   pure o

makeitwork :: Application → WinRtStep ()
makeitwork app = 
   (throwHResult =<<) . liftIO . 
      withForeignPtr app $ \p_app → do
         let p_insp = castPtr p_app
         struct ← peek p_insp
         vtbl :: Ptr (FunPtr (CInt → CInt)) ← peek struct
         alloca2 $ \p_iid_len p_iids → do
            let n = 4 * 8
            let getIids :: () = mkGetIIds $ (castPtrToFunPtr ∷ Ptr (FunPtr b) → FunPtr b) (vtbl `plusPtr` (24) )
            putStrLn "asking the iids"
            hres ← getIids p_insp p_iid_len p_iids
            print $ showHex hres ""
            when (hres < 0x8000_0000) $ do
               putStrLn "describing the iids"
               len ← peek p_iid_len
               print len
            return (hres, ())
 

      
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

textToHSTRING ∷ String → WinRtStep HSTRING
textToHSTRING t = do
   -- fpHstr is a pointer to a pointer
   hstr' ← (throwHResult =<<) . liftIO . 
      alloca $ \pHstr → 
         withCWStringLen t $ \(pTextBuf, len) → do
            hres ← c_WindowsCreateString pTextBuf (fromIntegral len)  pHstr
            hstr' ← peek pHstr
            return (hres, hstr')
   -- okay, so if we're here nothing went wrong
   -- lets attach a finaliser and wrap it up
   hstr'fp ← liftIO $
      --cfp_WindowsDeleteString ← mkWindowsDeleteString 
      --   c_WindowsDeleteString
      newForeignPtr cfp_WindowsDeleteString hstr'
   pure $ HSTRING hstr'fp

foreign import ccall "wrapper"
   mkWindowsDeleteString :: (HSTRING' → IO CUInt) 
                          → IO (FunPtr (HSTRING' → IO CUInt))


activateInstance ∷ HSTRING → WinRtStep Inspectable
activateInstance (HSTRING fp_appClassName) = do
   inspApp ← (throwHResult =<<) . liftIO .
      withForeignPtr fp_appClassName $ \p_appClassName → 
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
   
