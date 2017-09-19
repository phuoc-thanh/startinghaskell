### 1. Install Haskell platform

Win 32bit:

https://haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-i386-setup.exe

Win 64bit:

https://haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-x86_64-setup.exe

### 2. Install missing packages

> cabal update

> cabal install network http-conduit base16-bytestring split aeson-pretty
