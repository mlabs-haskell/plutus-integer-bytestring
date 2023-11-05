{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BSUtils (copyBytes, copyBytesReverse) where

import Data.Functor (void)
import Data.Word (Word8)
import Foreign.C.Types (CSize (CSize), CUChar)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Exts (ByteArray#, Int)
import GHC.Real (fromIntegral)
import System.IO (IO)

-- | Copies the specified number of bytes (which must be a positive number) from
-- the 'ByteArray#' argument (starting at the beginning) to the 'Ptr' 'Word8'
-- argument (starting at the beginning).
--
-- = Note
--
-- This calls @memcpy@ from the C standard library. We can't use
-- 'Foreign.Marshal.Utils.copyBytes', as it expects 'Ptr' arguments for both
-- source and destination, and we cannot (safely) convert a 'ByteArray#' into a
-- pointer unless we know that it's pinned. In our use case (working with
-- underlying 'Integer' representations), we /know/ that the 'ByteArray#' is
-- unpinned, which makes it dangerous to convert to a pointer. We are, however,
-- rescued by @UnliftedFFITypes@, which allows us two capabilities:
--
-- 1. We can pass 'ByteArray#' directly to the FFI, which will convert to an
--    @void*@ in our case.
-- 2. The runtime will temporarily ensure that the memory backing the
--    'ByteArray#' is pinned while the FFI call is in progress, making this
--    usage safe regardless of whether the 'ByteArray#' is pinned or not.
copyBytes ::
  Ptr Word8 ->
  ByteArray# ->
  Int ->
  IO ()
copyBytes ptr ba# len =
  void (cMemcpy (castPtr ptr) ba# (fromIntegral len))

-- | Copies the specified number of bytes (which must be a positive number) from
-- the 'ByteArray#' argument to the 'Ptr' Word8' argument (starting at the
-- end). Where the \'end\' is will be determined by the second 'Int' argument,
-- and the bytes will be copied in reverse.
copyBytesReverse ::
  Ptr Word8 ->
  ByteArray# ->
  Int ->
  Int ->
  IO ()
copyBytesReverse ptr ba# len end =
  cMemcpyR (castPtr ptr) ba# (fromIntegral len) (fromIntegral end)

foreign import ccall "string.h memcpy"
  cMemcpy ::
    Ptr CUChar ->
    ByteArray# ->
    CSize ->
    IO (Ptr Word8)

foreign import ccall "bsutils.h memcpy_r"
  cMemcpyR ::
    Ptr CUChar ->
    ByteArray# ->
    CSize ->
    CSize ->
    IO ()
