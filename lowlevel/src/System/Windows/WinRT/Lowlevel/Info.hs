module System.Windows.WinRT.Lowlevel.Info
where

import Foreign
import System.Windows.WinRT.Lowlevel.Monad

sizeOfMethodPtr :: Int
sizeOfMethodPtr = sizeOf (undefined :: FunPtr a)

sizeOfPtr :: Int
sizeOfPtr = sizeOf (undefined :: Ptr a)

alloca2 ∷ (Storable a, Storable b) ⇒ (Ptr a → Ptr b → IO z) → IO z
alloca2 f = alloca $ \ p1 → alloca $ \p2 → f p1 p2

alloca2wrt ∷ (Storable a, Storable b) ⇒ (Ptr a → Ptr b → WinRT z) → WinRT z
alloca2wrt = passthrough2 alloca2

alloca3 ∷ (Storable a, Storable b, Storable c) ⇒ (Ptr a → Ptr b → Ptr c → IO z) → IO z
alloca3 f = alloca $ \ p1 → alloca $ \p2 → alloca $ \p3 → f p1 p2 p3

alloca3wrt ∷ (Storable a, Storable b, Storable c) ⇒ (Ptr a → Ptr b → Ptr c → WinRT z) → WinRT z
alloca3wrt = passthrough3 alloca3