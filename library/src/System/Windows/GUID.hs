{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Windows.GUID where

import Foreign
import Numeric

newtype IID a = IID GUID
   deriving (Storable)

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

instance Show GUID where
   show (GUID d1 d2 d3 d4_0 d4_1 d4_2 d4_3 d4_4 d4_5 d4_6 d4_7) =
      showHex d1 "-" <>
      showHex d2 "-" <>
      showHex d3 "-" <>
      showHex d4_0 (showHex d4_1 "-") <>
      (showHex d4_2 . 
       showHex d4_3 . 
       showHex d4_4 . 
       showHex d4_5 . 
       showHex d4_6 . 
       showHex d4_7 $ "")

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