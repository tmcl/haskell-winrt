{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where
 
import Test.HUnit
import Foreign hiding (void)
import Data.IORef
import System.Windows.WinRT.IUnknown
import System.Mem
import System.Mem.Weak
import System.Exit
import Foreign.C.Types
import Control.Monad

type UnknownInfo = 
   (IORef IUnknownImplState, 
   ForeignPtr ITestType)


main :: IO ()
main = do
   res ← runTestTT $
      TestList [
         TestLabel "initially, it exists once created" test1, 
         TestLabel "add ref (pure hs)" test2,
         TestLabel "release (pure hs)" test3,
         TestLabel "release until freed (pure hs)" test3]
   unless (errors res == 0 && failures res == 0) 
      exitFailure

setupWorldFP :: IO (ForeignPtr ITestType)
setupWorldFP = do
   iunk ← newIUnknownImpl
   fp ← mallocForeignPtr
   withForeignPtr fp (`poke` iunk)
   return fp

setupWorldW :: IO (Weak UnknownInfo)
setupWorldW = do
   IUnknownImpl {..} ← newIUnknownImpl
   iunk ← deRefStablePtr impl_stbl
   let 
      the_ioref = impld_ioref iunk
      the_foreignPtr = impld_fmem iunk
   yieldWeak (the_ioref, the_foreignPtr)

yieldWeak :: UnknownInfo → IO (Weak UnknownInfo)
yieldWeak info = mkWeakPtr info Nothing

test1 :: Test
test1 = TestCase $ do
   fp ← setupWorldFP
   performMajorGC
   info ← assertExistsFP fp 
   assertRefCount 1 info

test2 :: Test
test2 = TestCase $ do
   weak ← setupWorldW
   strong ← assertExists weak
   res ← withForeignPtr (snd strong) addRefInternal
   assertEqual "surprising value returned from addRef" 2 res
   assertRefCount 2 strong

test3 :: Test
test3 = TestCase $ do
   fp ← setupWorldFP
   strong ← assertExistsFP fp
   res ← withForeignPtr (snd strong) $ \ptr → do
      void $ addRefInternal ptr
      releaseInternal ptr
   assertEqual "surprising value returned from addRef" 1 res
   assertRefCount 1 strong
   
assertExists :: Weak UnknownInfo → IO UnknownInfo
assertExists weak = do
   maybeIt ← deRefWeak weak
   case maybeIt of
      Just it → return it
      Nothing → assertFailure "I hoped to find it, but it doesn't exist"


assertExistsFP :: ForeignPtr ITestType → IO UnknownInfo
assertExistsFP fp = do
   IUnknownImpl {..} ← withForeignPtr fp peek
   IUnknownImplData {..} ← deRefStablePtr impl_stbl
   return (impld_ioref, impld_fmem)

assertRefCount :: CULong → UnknownInfo → IO ()
assertRefCount count strong = do
   state ← readIORef $ fst strong
   assertEqual "surprising ref count." count (refsCount state)

data TestType = TestType
type ITestType = IUnknownImpl TestType

instance HasVtbl TestType where
   newtype VTable TestType = TestVtbl IUnknownVtbl 
      deriving (Storable)
   mkVtbl addRef release = do
      qi ← mkDefaultQueryInterface [uniid iid]
      return $ TestVtbl $ iUnknownVtbl qi addRef release
  -- we don't use {..} here since we want to free them _all_,
  -- even if code changes in the future to create more
   freeFunPtrs (TestVtbl (IUnknownVtbl a b c)) = do
      freeHaskellFunPtr a
      freeHaskellFunPtr b
      freeHaskellFunPtr c