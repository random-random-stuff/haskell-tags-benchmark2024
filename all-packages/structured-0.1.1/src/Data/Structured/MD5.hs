{-# LANGUAGE Trustworthy #-}
module Data.Structured.MD5 (
    MD5,
    showMD5,
    md5,
    md5FromInteger,
    ) where

import Data.Bits        (complement, shiftR, (.&.))
import Foreign.Ptr      (castPtr)
import GHC.Fingerprint  (Fingerprint (..), fingerprintData)
import Numeric          (showHex)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS

type MD5 = Fingerprint

-- | Show 'MD5' in human readable form
--
-- >>> showMD5 (Fingerprint 123 456)
-- "000000000000007b00000000000001c8"
--
-- >>> showMD5 $ md5 $ BS.pack [0..127]
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- @since  3.2.0.0
showMD5 :: MD5 -> String
showMD5 (Fingerprint a b) = pad a' ++ pad b' where
    a' = showHex a ""
    b' = showHex b ""
    pad s = replicate (16 - length s) '0' ++ s

-- | @since  3.2.0.0
md5 :: BS.ByteString -> MD5
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fingerprintData (castPtr ptr) len

-- |
--
-- >>> showMD5 $ md5FromInteger 0x37eff01866ba3f538421b30b7cbefcac
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- Note: the input is truncated:
--
-- >>> showMD5 $ md5FromInteger 0x1230000037eff01866ba3f538421b30b7cbefcac
-- "37eff01866ba3f538421b30b7cbefcac"
--
-- Yet, negative numbers are not a problem...
--
-- >>> showMD5 $ md5FromInteger (-1)
-- "ffffffffffffffffffffffffffffffff"
--
-- @since 3.4.0.0
md5FromInteger :: Integer -> MD5
md5FromInteger i = Fingerprint hi lo where
    mask = complement 0
    lo   = mask .&. fromInteger i
    hi   = mask .&. fromInteger (i `shiftR` 64)
