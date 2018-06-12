{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module LFSR (
    LFSR, Word32(..),
    newLFSR, getLFSR, setLFSR, stepLFSR, repeatLFSR, avgLFSR,
    LFSRStruct'(..), repeatSteps,
) where

import           Control.Monad.ST        (ST, runST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.Word               (Word32)
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Storable


data LFSRStruct     -- | empty type

data LFSR = LFSR
        (ForeignPtr LFSRStruct) -- | Pointer to C struct (malloc'ed)
    deriving Show

foreign import ccall unsafe "lfsr.h new_lfsr"
    newLFSR' :: IO (Ptr LFSRStruct)

foreign import ccall unsafe "lfsr.h delete_lfsr"
    deleteLFSR' :: Ptr LFSRStruct -> IO ()

foreign import ccall unsafe "lfsr.h set_lfsr"
    setLFSR' :: Ptr LFSRStruct -> Word32 -> IO ()

foreign import ccall unsafe "lfsr.h get_lfsr"
    getLFSR' :: Ptr LFSRStruct -> IO (Word32)

foreign import ccall unsafe "lfsr.h step_lfsr"
    stepLFSR' :: Ptr LFSRStruct -> IO ()

foreign import ccall unsafe "lfsr.h repeat_lfsr"
    repeatLFSR' :: Ptr LFSRStruct -> Int -> IO ()

foreign import ccall unsafe "lfsr.h avg"
    avgLFSR' :: Ptr LFSRStruct -> Int -> IO Double

data LFSRStruct' = LFSRStruct' CUInt
    deriving (Show, Read, Eq)

newLFSR :: IO LFSR
newLFSR = do
    p <- newLFSR'
    fp <- newForeignPtr finalizerFree p
    return $ LFSR fp
{-# INLINE newLFSR #-}

getLFSR :: LFSR -> IO Word32
getLFSR (LFSR p) = withForeignPtr p $ \p'->getLFSR' p'
{-# INLINE getLFSR #-}

setLFSR :: LFSR -> Word32 -> IO ()
setLFSR (LFSR p) !v = withForeignPtr p $ \p'->setLFSR' p' v
{-# INLINE setLFSR #-}

stepLFSR :: LFSR -> IO ()
stepLFSR (LFSR p) = withForeignPtr p $ \p'->stepLFSR' p'
{-# INLINE stepLFSR #-}

repeatLFSR :: LFSR -> Int -> IO ()
repeatLFSR (LFSR p) !n = withForeignPtr p $ \p'->repeatLFSR' p' n
{-# INLINE repeatLFSR #-}

avgLFSR :: LFSR -> Int -> IO Double
avgLFSR (LFSR p) !n = withForeignPtr p $ \p'->avgLFSR' p' n
{-# INLINE avgLFSR #-}

{----------------------- Avoid ForeignPtr by using malloca -----------------------}

instance Storable LFSRStruct' where
    sizeOf _ = 4
    alignment = sizeOf
    peek ptr = do
        a <- peekByteOff ptr 0
        return (LFSRStruct' a)

foreign import ccall unsafe "lfsr.h set_lfsr"
    setLFSR2' :: Ptr LFSRStruct' -> Word32 -> IO ()

foreign import ccall unsafe "lfsr.h get_lfsr"
    getLFSR2' :: Ptr LFSRStruct' -> IO Word32

foreign import ccall unsafe "lfsr.h step_lfsr"
    stepLFSR2' :: Ptr LFSRStruct' -> IO ()

setLFSR2 :: (Ptr LFSRStruct') -> Word32 -> IO ()
setLFSR2 p !v = setLFSR2' p v
{-# INLINE setLFSR2 #-}

getLFSR2 :: (Ptr LFSRStruct') -> IO Word32
getLFSR2 p = getLFSR2' p
{-# INLINE getLFSR2 #-}

stepLFSR2 :: (Ptr LFSRStruct') -> IO ()
stepLFSR2 p = stepLFSR2' p
{-# INLINE stepLFSR2 #-}

repeatSteps :: Word32 -> Int -> IO Word32
repeatSteps start n = alloca rep' where
    rep' :: Ptr LFSRStruct' -> IO Word32
    rep' p = do
        setLFSR2 p start
        (sequence_ . (replicate n)) (stepLFSR2 p)
        getLFSR2 p

