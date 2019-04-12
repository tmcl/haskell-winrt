Haskell WinRT binding
=====================

These don't do anything yet. It's just a giant file with some experimentation.
So far I can get a pointer to an instance of Application. I don't think that's
interesting, but I can do it.

Don'ts
------

Don't use the old COM/HDirect libraries, since they're unreadable and based
around some principles of FFI that are now dead.

Don't make use of any package that insn't installed with the Haskell
Platform, since Nix isn't on Windows.
