{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources.
-}

module System.EntropyWindows
        ( CryptHandle
        , openHandle
        , hGetEntropy
        , closeHandle
        , hardwareRandom
        ) where

import Control.Monad (liftM, when)
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)
import System.Win32.Types (ULONG_PTR, errorWin)
import Foreign (allocaBytes)
import Data.ByteString as B
import Data.ByteString.Internal as BI
import Data.Int (Int32)
import Data.Bits (xor)
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek)

-- C example for windows rng - taken from a blog, can't recall which one but thank you!
--      #include <Windows.h>
--      #include <Wincrypt.h>
--      ...
--      //
--      // DISCLAIMER: Don't forget to check your error codes!!
--      // I am not checking as to make the example simple...
--      //
--      HCRYPTPROV hCryptCtx = NULL;
--      BYTE randomArray[128];
--
--      CryptAcquireContext(&hCryptCtx, NULL, MS_DEF_PROV, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
--      CryptGenRandom(hCryptCtx, 128, randomArray);
--      CryptReleaseContext(hCryptCtx, 0);


#ifdef arch_i386
-- See .cabal wrt GCC 4.8.2 asm compilation bug
#undef HAVE_RDRAND
#endif

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif

type HCRYPTPROV = ULONG_PTR
data CryptHandle
    = CH HCRYPTPROV


-- | Get random values from the hardware RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Supported hardware:
--      * RDRAND
--      * Patches welcome
hardwareRandom :: Int -> IO (Maybe B.ByteString)
#ifdef HAVE_RDRAND
hardwareRandom n =
  do b <- cpuHasRdRand
     if b
        then Just <$> BI.create n (\ptr ->
                        do r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                           when (r /= 0) (fail "RDRand failed to gather entropy"))
        else pure Nothing
#else
hardwareRandom _ = pure Nothing
#endif

-- Define the constants we need from WinCrypt.h
msDefProv :: String
msDefProv = "Microsoft Base Cryptographic Provider v1.0"
provRSAFull :: Word32
provRSAFull = 1
cryptVerifyContext :: Word32
cryptVerifyContext = fromIntegral 0xF0000000

-- Declare the required CryptoAPI imports
foreign import stdcall unsafe "CryptAcquireContextA"
   c_cryptAcquireCtx :: Ptr HCRYPTPROV -> CString -> CString -> Word32 -> Word32 -> IO Int32
foreign import stdcall unsafe "CryptGenRandom"
   c_cryptGenRandom :: HCRYPTPROV -> Word32 -> Ptr Word8 -> IO Int32
foreign import stdcall unsafe "CryptReleaseContext"
   c_cryptReleaseCtx :: HCRYPTPROV -> Word32 -> IO Int32

cryptAcquireCtx :: IO HCRYPTPROV
cryptAcquireCtx =
   alloca $ \handlePtr ->
      withCString msDefProv $ \provName -> do
         stat <- c_cryptAcquireCtx handlePtr nullPtr provName provRSAFull cryptVerifyContext
         if (toBool stat)
            then peek handlePtr
            else errorWin "c_cryptAcquireCtx"

cryptGenRandom :: HCRYPTPROV -> Int -> IO B.ByteString
cryptGenRandom h i =
   BI.create i $ \c_buffer -> do
      stat <- c_cryptGenRandom h (fromIntegral i) c_buffer
      if (toBool stat)
         then return ()
         else errorWin "c_cryptGenRandom"

cryptReleaseCtx :: HCRYPTPROV -> IO ()
cryptReleaseCtx h = do
   stat <- c_cryptReleaseCtx h 0
   if (toBool stat)
      then return ()
      else errorWin "c_cryptReleaseCtx"

-- |Open a handle from which random data can be read
openHandle :: IO CryptHandle
openHandle = CH `fmap` cryptAcquireCtx

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH h)        = cryptReleaseCtx h

-- |Read from `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) n = cryptGenRandom h n
