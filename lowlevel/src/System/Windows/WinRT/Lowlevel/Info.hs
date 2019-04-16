module System.Windows.WinRT.Lowlevel.Info
where

import Foreign
import System.Windows.WinRT.Lowlevel.Monad

sizeOfMethodPtr = sizeOf (undefined :: FunPtr a)

alloca2 ∷ (Storable a, Storable b) ⇒ (Ptr a → Ptr b → IO c) → IO c
alloca2 f = alloca $ \ p1 → alloca $ \p2 → f p1 p2

alloca2' ∷ (Storable a, Storable b) ⇒ (Ptr a → Ptr b → WinRT c) → WinRT c
alloca2' = passthrough2 alloca2