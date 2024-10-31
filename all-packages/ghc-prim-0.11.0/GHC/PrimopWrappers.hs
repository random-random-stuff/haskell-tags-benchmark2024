-- | Users should not import this module.  It is GHC internal only.
-- Use "GHC.Exts" instead.
{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-deprecations -O0 -fno-do-eta-reduction #-}
module GHC.PrimopWrappers where
import qualified GHC.Prim
import GHC.Tuple ()
import GHC.Prim (Char#, Int#, Int8#, Word8#, Word#, Int16#, Word16#, Int32#, Word32#, Int64#, Word64#, Float#, Double#, State#, MutableArray#, Array#, SmallMutableArray#, SmallArray#, MutableByteArray#, ByteArray#, Addr#, StablePtr#, RealWorld, MutVar#, PromptTag#, TVar#, MVar#, IOPort#, ThreadId#, Weak#, StableName#, Compact#, BCO)
{-# NOINLINE gtChar# #-}
gtChar# :: Char# -> Char# -> Int#
gtChar# a1 a2 = GHC.Prim.gtChar# a1 a2
{-# NOINLINE geChar# #-}
geChar# :: Char# -> Char# -> Int#
geChar# a1 a2 = GHC.Prim.geChar# a1 a2
{-# NOINLINE eqChar# #-}
eqChar# :: Char# -> Char# -> Int#
eqChar# a1 a2 = GHC.Prim.eqChar# a1 a2
{-# NOINLINE neChar# #-}
neChar# :: Char# -> Char# -> Int#
neChar# a1 a2 = GHC.Prim.neChar# a1 a2
{-# NOINLINE ltChar# #-}
ltChar# :: Char# -> Char# -> Int#
ltChar# a1 a2 = GHC.Prim.ltChar# a1 a2
{-# NOINLINE leChar# #-}
leChar# :: Char# -> Char# -> Int#
leChar# a1 a2 = GHC.Prim.leChar# a1 a2
{-# NOINLINE ord# #-}
ord# :: Char# -> Int#
ord# a1 = GHC.Prim.ord# a1
{-# NOINLINE int8ToInt# #-}
int8ToInt# :: Int8# -> Int#
int8ToInt# a1 = GHC.Prim.int8ToInt# a1
{-# NOINLINE intToInt8# #-}
intToInt8# :: Int# -> Int8#
intToInt8# a1 = GHC.Prim.intToInt8# a1
{-# NOINLINE negateInt8# #-}
negateInt8# :: Int8# -> Int8#
negateInt8# a1 = GHC.Prim.negateInt8# a1
{-# NOINLINE plusInt8# #-}
plusInt8# :: Int8# -> Int8# -> Int8#
plusInt8# a1 a2 = GHC.Prim.plusInt8# a1 a2
{-# NOINLINE subInt8# #-}
subInt8# :: Int8# -> Int8# -> Int8#
subInt8# a1 a2 = GHC.Prim.subInt8# a1 a2
{-# NOINLINE timesInt8# #-}
timesInt8# :: Int8# -> Int8# -> Int8#
timesInt8# a1 a2 = GHC.Prim.timesInt8# a1 a2
{-# NOINLINE quotInt8# #-}
quotInt8# :: Int8# -> Int8# -> Int8#
quotInt8# a1 a2 = GHC.Prim.quotInt8# a1 a2
{-# NOINLINE remInt8# #-}
remInt8# :: Int8# -> Int8# -> Int8#
remInt8# a1 a2 = GHC.Prim.remInt8# a1 a2
{-# NOINLINE quotRemInt8# #-}
quotRemInt8# :: Int8# -> Int8# -> (# Int8#,Int8# #)
quotRemInt8# a1 a2 = GHC.Prim.quotRemInt8# a1 a2
{-# NOINLINE uncheckedShiftLInt8# #-}
uncheckedShiftLInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftLInt8# a1 a2 = GHC.Prim.uncheckedShiftLInt8# a1 a2
{-# NOINLINE uncheckedShiftRAInt8# #-}
uncheckedShiftRAInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftRAInt8# a1 a2 = GHC.Prim.uncheckedShiftRAInt8# a1 a2
{-# NOINLINE uncheckedShiftRLInt8# #-}
uncheckedShiftRLInt8# :: Int8# -> Int# -> Int8#
uncheckedShiftRLInt8# a1 a2 = GHC.Prim.uncheckedShiftRLInt8# a1 a2
{-# NOINLINE int8ToWord8# #-}
int8ToWord8# :: Int8# -> Word8#
int8ToWord8# a1 = GHC.Prim.int8ToWord8# a1
{-# NOINLINE eqInt8# #-}
eqInt8# :: Int8# -> Int8# -> Int#
eqInt8# a1 a2 = GHC.Prim.eqInt8# a1 a2
{-# NOINLINE geInt8# #-}
geInt8# :: Int8# -> Int8# -> Int#
geInt8# a1 a2 = GHC.Prim.geInt8# a1 a2
{-# NOINLINE gtInt8# #-}
gtInt8# :: Int8# -> Int8# -> Int#
gtInt8# a1 a2 = GHC.Prim.gtInt8# a1 a2
{-# NOINLINE leInt8# #-}
leInt8# :: Int8# -> Int8# -> Int#
leInt8# a1 a2 = GHC.Prim.leInt8# a1 a2
{-# NOINLINE ltInt8# #-}
ltInt8# :: Int8# -> Int8# -> Int#
ltInt8# a1 a2 = GHC.Prim.ltInt8# a1 a2
{-# NOINLINE neInt8# #-}
neInt8# :: Int8# -> Int8# -> Int#
neInt8# a1 a2 = GHC.Prim.neInt8# a1 a2
{-# NOINLINE word8ToWord# #-}
word8ToWord# :: Word8# -> Word#
word8ToWord# a1 = GHC.Prim.word8ToWord# a1
{-# NOINLINE wordToWord8# #-}
wordToWord8# :: Word# -> Word8#
wordToWord8# a1 = GHC.Prim.wordToWord8# a1
{-# NOINLINE plusWord8# #-}
plusWord8# :: Word8# -> Word8# -> Word8#
plusWord8# a1 a2 = GHC.Prim.plusWord8# a1 a2
{-# NOINLINE subWord8# #-}
subWord8# :: Word8# -> Word8# -> Word8#
subWord8# a1 a2 = GHC.Prim.subWord8# a1 a2
{-# NOINLINE timesWord8# #-}
timesWord8# :: Word8# -> Word8# -> Word8#
timesWord8# a1 a2 = GHC.Prim.timesWord8# a1 a2
{-# NOINLINE quotWord8# #-}
quotWord8# :: Word8# -> Word8# -> Word8#
quotWord8# a1 a2 = GHC.Prim.quotWord8# a1 a2
{-# NOINLINE remWord8# #-}
remWord8# :: Word8# -> Word8# -> Word8#
remWord8# a1 a2 = GHC.Prim.remWord8# a1 a2
{-# NOINLINE quotRemWord8# #-}
quotRemWord8# :: Word8# -> Word8# -> (# Word8#,Word8# #)
quotRemWord8# a1 a2 = GHC.Prim.quotRemWord8# a1 a2
{-# NOINLINE andWord8# #-}
andWord8# :: Word8# -> Word8# -> Word8#
andWord8# a1 a2 = GHC.Prim.andWord8# a1 a2
{-# NOINLINE orWord8# #-}
orWord8# :: Word8# -> Word8# -> Word8#
orWord8# a1 a2 = GHC.Prim.orWord8# a1 a2
{-# NOINLINE xorWord8# #-}
xorWord8# :: Word8# -> Word8# -> Word8#
xorWord8# a1 a2 = GHC.Prim.xorWord8# a1 a2
{-# NOINLINE notWord8# #-}
notWord8# :: Word8# -> Word8#
notWord8# a1 = GHC.Prim.notWord8# a1
{-# NOINLINE uncheckedShiftLWord8# #-}
uncheckedShiftLWord8# :: Word8# -> Int# -> Word8#
uncheckedShiftLWord8# a1 a2 = GHC.Prim.uncheckedShiftLWord8# a1 a2
{-# NOINLINE uncheckedShiftRLWord8# #-}
uncheckedShiftRLWord8# :: Word8# -> Int# -> Word8#
uncheckedShiftRLWord8# a1 a2 = GHC.Prim.uncheckedShiftRLWord8# a1 a2
{-# NOINLINE word8ToInt8# #-}
word8ToInt8# :: Word8# -> Int8#
word8ToInt8# a1 = GHC.Prim.word8ToInt8# a1
{-# NOINLINE eqWord8# #-}
eqWord8# :: Word8# -> Word8# -> Int#
eqWord8# a1 a2 = GHC.Prim.eqWord8# a1 a2
{-# NOINLINE geWord8# #-}
geWord8# :: Word8# -> Word8# -> Int#
geWord8# a1 a2 = GHC.Prim.geWord8# a1 a2
{-# NOINLINE gtWord8# #-}
gtWord8# :: Word8# -> Word8# -> Int#
gtWord8# a1 a2 = GHC.Prim.gtWord8# a1 a2
{-# NOINLINE leWord8# #-}
leWord8# :: Word8# -> Word8# -> Int#
leWord8# a1 a2 = GHC.Prim.leWord8# a1 a2
{-# NOINLINE ltWord8# #-}
ltWord8# :: Word8# -> Word8# -> Int#
ltWord8# a1 a2 = GHC.Prim.ltWord8# a1 a2
{-# NOINLINE neWord8# #-}
neWord8# :: Word8# -> Word8# -> Int#
neWord8# a1 a2 = GHC.Prim.neWord8# a1 a2
{-# NOINLINE int16ToInt# #-}
int16ToInt# :: Int16# -> Int#
int16ToInt# a1 = GHC.Prim.int16ToInt# a1
{-# NOINLINE intToInt16# #-}
intToInt16# :: Int# -> Int16#
intToInt16# a1 = GHC.Prim.intToInt16# a1
{-# NOINLINE negateInt16# #-}
negateInt16# :: Int16# -> Int16#
negateInt16# a1 = GHC.Prim.negateInt16# a1
{-# NOINLINE plusInt16# #-}
plusInt16# :: Int16# -> Int16# -> Int16#
plusInt16# a1 a2 = GHC.Prim.plusInt16# a1 a2
{-# NOINLINE subInt16# #-}
subInt16# :: Int16# -> Int16# -> Int16#
subInt16# a1 a2 = GHC.Prim.subInt16# a1 a2
{-# NOINLINE timesInt16# #-}
timesInt16# :: Int16# -> Int16# -> Int16#
timesInt16# a1 a2 = GHC.Prim.timesInt16# a1 a2
{-# NOINLINE quotInt16# #-}
quotInt16# :: Int16# -> Int16# -> Int16#
quotInt16# a1 a2 = GHC.Prim.quotInt16# a1 a2
{-# NOINLINE remInt16# #-}
remInt16# :: Int16# -> Int16# -> Int16#
remInt16# a1 a2 = GHC.Prim.remInt16# a1 a2
{-# NOINLINE quotRemInt16# #-}
quotRemInt16# :: Int16# -> Int16# -> (# Int16#,Int16# #)
quotRemInt16# a1 a2 = GHC.Prim.quotRemInt16# a1 a2
{-# NOINLINE uncheckedShiftLInt16# #-}
uncheckedShiftLInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftLInt16# a1 a2 = GHC.Prim.uncheckedShiftLInt16# a1 a2
{-# NOINLINE uncheckedShiftRAInt16# #-}
uncheckedShiftRAInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftRAInt16# a1 a2 = GHC.Prim.uncheckedShiftRAInt16# a1 a2
{-# NOINLINE uncheckedShiftRLInt16# #-}
uncheckedShiftRLInt16# :: Int16# -> Int# -> Int16#
uncheckedShiftRLInt16# a1 a2 = GHC.Prim.uncheckedShiftRLInt16# a1 a2
{-# NOINLINE int16ToWord16# #-}
int16ToWord16# :: Int16# -> Word16#
int16ToWord16# a1 = GHC.Prim.int16ToWord16# a1
{-# NOINLINE eqInt16# #-}
eqInt16# :: Int16# -> Int16# -> Int#
eqInt16# a1 a2 = GHC.Prim.eqInt16# a1 a2
{-# NOINLINE geInt16# #-}
geInt16# :: Int16# -> Int16# -> Int#
geInt16# a1 a2 = GHC.Prim.geInt16# a1 a2
{-# NOINLINE gtInt16# #-}
gtInt16# :: Int16# -> Int16# -> Int#
gtInt16# a1 a2 = GHC.Prim.gtInt16# a1 a2
{-# NOINLINE leInt16# #-}
leInt16# :: Int16# -> Int16# -> Int#
leInt16# a1 a2 = GHC.Prim.leInt16# a1 a2
{-# NOINLINE ltInt16# #-}
ltInt16# :: Int16# -> Int16# -> Int#
ltInt16# a1 a2 = GHC.Prim.ltInt16# a1 a2
{-# NOINLINE neInt16# #-}
neInt16# :: Int16# -> Int16# -> Int#
neInt16# a1 a2 = GHC.Prim.neInt16# a1 a2
{-# NOINLINE word16ToWord# #-}
word16ToWord# :: Word16# -> Word#
word16ToWord# a1 = GHC.Prim.word16ToWord# a1
{-# NOINLINE wordToWord16# #-}
wordToWord16# :: Word# -> Word16#
wordToWord16# a1 = GHC.Prim.wordToWord16# a1
{-# NOINLINE plusWord16# #-}
plusWord16# :: Word16# -> Word16# -> Word16#
plusWord16# a1 a2 = GHC.Prim.plusWord16# a1 a2
{-# NOINLINE subWord16# #-}
subWord16# :: Word16# -> Word16# -> Word16#
subWord16# a1 a2 = GHC.Prim.subWord16# a1 a2
{-# NOINLINE timesWord16# #-}
timesWord16# :: Word16# -> Word16# -> Word16#
timesWord16# a1 a2 = GHC.Prim.timesWord16# a1 a2
{-# NOINLINE quotWord16# #-}
quotWord16# :: Word16# -> Word16# -> Word16#
quotWord16# a1 a2 = GHC.Prim.quotWord16# a1 a2
{-# NOINLINE remWord16# #-}
remWord16# :: Word16# -> Word16# -> Word16#
remWord16# a1 a2 = GHC.Prim.remWord16# a1 a2
{-# NOINLINE quotRemWord16# #-}
quotRemWord16# :: Word16# -> Word16# -> (# Word16#,Word16# #)
quotRemWord16# a1 a2 = GHC.Prim.quotRemWord16# a1 a2
{-# NOINLINE andWord16# #-}
andWord16# :: Word16# -> Word16# -> Word16#
andWord16# a1 a2 = GHC.Prim.andWord16# a1 a2
{-# NOINLINE orWord16# #-}
orWord16# :: Word16# -> Word16# -> Word16#
orWord16# a1 a2 = GHC.Prim.orWord16# a1 a2
{-# NOINLINE xorWord16# #-}
xorWord16# :: Word16# -> Word16# -> Word16#
xorWord16# a1 a2 = GHC.Prim.xorWord16# a1 a2
{-# NOINLINE notWord16# #-}
notWord16# :: Word16# -> Word16#
notWord16# a1 = GHC.Prim.notWord16# a1
{-# NOINLINE uncheckedShiftLWord16# #-}
uncheckedShiftLWord16# :: Word16# -> Int# -> Word16#
uncheckedShiftLWord16# a1 a2 = GHC.Prim.uncheckedShiftLWord16# a1 a2
{-# NOINLINE uncheckedShiftRLWord16# #-}
uncheckedShiftRLWord16# :: Word16# -> Int# -> Word16#
uncheckedShiftRLWord16# a1 a2 = GHC.Prim.uncheckedShiftRLWord16# a1 a2
{-# NOINLINE word16ToInt16# #-}
word16ToInt16# :: Word16# -> Int16#
word16ToInt16# a1 = GHC.Prim.word16ToInt16# a1
{-# NOINLINE eqWord16# #-}
eqWord16# :: Word16# -> Word16# -> Int#
eqWord16# a1 a2 = GHC.Prim.eqWord16# a1 a2
{-# NOINLINE geWord16# #-}
geWord16# :: Word16# -> Word16# -> Int#
geWord16# a1 a2 = GHC.Prim.geWord16# a1 a2
{-# NOINLINE gtWord16# #-}
gtWord16# :: Word16# -> Word16# -> Int#
gtWord16# a1 a2 = GHC.Prim.gtWord16# a1 a2
{-# NOINLINE leWord16# #-}
leWord16# :: Word16# -> Word16# -> Int#
leWord16# a1 a2 = GHC.Prim.leWord16# a1 a2
{-# NOINLINE ltWord16# #-}
ltWord16# :: Word16# -> Word16# -> Int#
ltWord16# a1 a2 = GHC.Prim.ltWord16# a1 a2
{-# NOINLINE neWord16# #-}
neWord16# :: Word16# -> Word16# -> Int#
neWord16# a1 a2 = GHC.Prim.neWord16# a1 a2
{-# NOINLINE int32ToInt# #-}
int32ToInt# :: Int32# -> Int#
int32ToInt# a1 = GHC.Prim.int32ToInt# a1
{-# NOINLINE intToInt32# #-}
intToInt32# :: Int# -> Int32#
intToInt32# a1 = GHC.Prim.intToInt32# a1
{-# NOINLINE negateInt32# #-}
negateInt32# :: Int32# -> Int32#
negateInt32# a1 = GHC.Prim.negateInt32# a1
{-# NOINLINE plusInt32# #-}
plusInt32# :: Int32# -> Int32# -> Int32#
plusInt32# a1 a2 = GHC.Prim.plusInt32# a1 a2
{-# NOINLINE subInt32# #-}
subInt32# :: Int32# -> Int32# -> Int32#
subInt32# a1 a2 = GHC.Prim.subInt32# a1 a2
{-# NOINLINE timesInt32# #-}
timesInt32# :: Int32# -> Int32# -> Int32#
timesInt32# a1 a2 = GHC.Prim.timesInt32# a1 a2
{-# NOINLINE quotInt32# #-}
quotInt32# :: Int32# -> Int32# -> Int32#
quotInt32# a1 a2 = GHC.Prim.quotInt32# a1 a2
{-# NOINLINE remInt32# #-}
remInt32# :: Int32# -> Int32# -> Int32#
remInt32# a1 a2 = GHC.Prim.remInt32# a1 a2
{-# NOINLINE quotRemInt32# #-}
quotRemInt32# :: Int32# -> Int32# -> (# Int32#,Int32# #)
quotRemInt32# a1 a2 = GHC.Prim.quotRemInt32# a1 a2
{-# NOINLINE uncheckedShiftLInt32# #-}
uncheckedShiftLInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftLInt32# a1 a2 = GHC.Prim.uncheckedShiftLInt32# a1 a2
{-# NOINLINE uncheckedShiftRAInt32# #-}
uncheckedShiftRAInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftRAInt32# a1 a2 = GHC.Prim.uncheckedShiftRAInt32# a1 a2
{-# NOINLINE uncheckedShiftRLInt32# #-}
uncheckedShiftRLInt32# :: Int32# -> Int# -> Int32#
uncheckedShiftRLInt32# a1 a2 = GHC.Prim.uncheckedShiftRLInt32# a1 a2
{-# NOINLINE int32ToWord32# #-}
int32ToWord32# :: Int32# -> Word32#
int32ToWord32# a1 = GHC.Prim.int32ToWord32# a1
{-# NOINLINE eqInt32# #-}
eqInt32# :: Int32# -> Int32# -> Int#
eqInt32# a1 a2 = GHC.Prim.eqInt32# a1 a2
{-# NOINLINE geInt32# #-}
geInt32# :: Int32# -> Int32# -> Int#
geInt32# a1 a2 = GHC.Prim.geInt32# a1 a2
{-# NOINLINE gtInt32# #-}
gtInt32# :: Int32# -> Int32# -> Int#
gtInt32# a1 a2 = GHC.Prim.gtInt32# a1 a2
{-# NOINLINE leInt32# #-}
leInt32# :: Int32# -> Int32# -> Int#
leInt32# a1 a2 = GHC.Prim.leInt32# a1 a2
{-# NOINLINE ltInt32# #-}
ltInt32# :: Int32# -> Int32# -> Int#
ltInt32# a1 a2 = GHC.Prim.ltInt32# a1 a2
{-# NOINLINE neInt32# #-}
neInt32# :: Int32# -> Int32# -> Int#
neInt32# a1 a2 = GHC.Prim.neInt32# a1 a2
{-# NOINLINE word32ToWord# #-}
word32ToWord# :: Word32# -> Word#
word32ToWord# a1 = GHC.Prim.word32ToWord# a1
{-# NOINLINE wordToWord32# #-}
wordToWord32# :: Word# -> Word32#
wordToWord32# a1 = GHC.Prim.wordToWord32# a1
{-# NOINLINE plusWord32# #-}
plusWord32# :: Word32# -> Word32# -> Word32#
plusWord32# a1 a2 = GHC.Prim.plusWord32# a1 a2
{-# NOINLINE subWord32# #-}
subWord32# :: Word32# -> Word32# -> Word32#
subWord32# a1 a2 = GHC.Prim.subWord32# a1 a2
{-# NOINLINE timesWord32# #-}
timesWord32# :: Word32# -> Word32# -> Word32#
timesWord32# a1 a2 = GHC.Prim.timesWord32# a1 a2
{-# NOINLINE quotWord32# #-}
quotWord32# :: Word32# -> Word32# -> Word32#
quotWord32# a1 a2 = GHC.Prim.quotWord32# a1 a2
{-# NOINLINE remWord32# #-}
remWord32# :: Word32# -> Word32# -> Word32#
remWord32# a1 a2 = GHC.Prim.remWord32# a1 a2
{-# NOINLINE quotRemWord32# #-}
quotRemWord32# :: Word32# -> Word32# -> (# Word32#,Word32# #)
quotRemWord32# a1 a2 = GHC.Prim.quotRemWord32# a1 a2
{-# NOINLINE andWord32# #-}
andWord32# :: Word32# -> Word32# -> Word32#
andWord32# a1 a2 = GHC.Prim.andWord32# a1 a2
{-# NOINLINE orWord32# #-}
orWord32# :: Word32# -> Word32# -> Word32#
orWord32# a1 a2 = GHC.Prim.orWord32# a1 a2
{-# NOINLINE xorWord32# #-}
xorWord32# :: Word32# -> Word32# -> Word32#
xorWord32# a1 a2 = GHC.Prim.xorWord32# a1 a2
{-# NOINLINE notWord32# #-}
notWord32# :: Word32# -> Word32#
notWord32# a1 = GHC.Prim.notWord32# a1
{-# NOINLINE uncheckedShiftLWord32# #-}
uncheckedShiftLWord32# :: Word32# -> Int# -> Word32#
uncheckedShiftLWord32# a1 a2 = GHC.Prim.uncheckedShiftLWord32# a1 a2
{-# NOINLINE uncheckedShiftRLWord32# #-}
uncheckedShiftRLWord32# :: Word32# -> Int# -> Word32#
uncheckedShiftRLWord32# a1 a2 = GHC.Prim.uncheckedShiftRLWord32# a1 a2
{-# NOINLINE word32ToInt32# #-}
word32ToInt32# :: Word32# -> Int32#
word32ToInt32# a1 = GHC.Prim.word32ToInt32# a1
{-# NOINLINE eqWord32# #-}
eqWord32# :: Word32# -> Word32# -> Int#
eqWord32# a1 a2 = GHC.Prim.eqWord32# a1 a2
{-# NOINLINE geWord32# #-}
geWord32# :: Word32# -> Word32# -> Int#
geWord32# a1 a2 = GHC.Prim.geWord32# a1 a2
{-# NOINLINE gtWord32# #-}
gtWord32# :: Word32# -> Word32# -> Int#
gtWord32# a1 a2 = GHC.Prim.gtWord32# a1 a2
{-# NOINLINE leWord32# #-}
leWord32# :: Word32# -> Word32# -> Int#
leWord32# a1 a2 = GHC.Prim.leWord32# a1 a2
{-# NOINLINE ltWord32# #-}
ltWord32# :: Word32# -> Word32# -> Int#
ltWord32# a1 a2 = GHC.Prim.ltWord32# a1 a2
{-# NOINLINE neWord32# #-}
neWord32# :: Word32# -> Word32# -> Int#
neWord32# a1 a2 = GHC.Prim.neWord32# a1 a2
{-# NOINLINE int64ToInt# #-}
int64ToInt# :: Int64# -> Int#
int64ToInt# a1 = GHC.Prim.int64ToInt# a1
{-# NOINLINE intToInt64# #-}
intToInt64# :: Int# -> Int64#
intToInt64# a1 = GHC.Prim.intToInt64# a1
{-# NOINLINE negateInt64# #-}
negateInt64# :: Int64# -> Int64#
negateInt64# a1 = GHC.Prim.negateInt64# a1
{-# NOINLINE plusInt64# #-}
plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# a1 a2 = GHC.Prim.plusInt64# a1 a2
{-# NOINLINE subInt64# #-}
subInt64# :: Int64# -> Int64# -> Int64#
subInt64# a1 a2 = GHC.Prim.subInt64# a1 a2
{-# NOINLINE timesInt64# #-}
timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# a1 a2 = GHC.Prim.timesInt64# a1 a2
{-# NOINLINE quotInt64# #-}
quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# a1 a2 = GHC.Prim.quotInt64# a1 a2
{-# NOINLINE remInt64# #-}
remInt64# :: Int64# -> Int64# -> Int64#
remInt64# a1 a2 = GHC.Prim.remInt64# a1 a2
{-# NOINLINE uncheckedIShiftL64# #-}
uncheckedIShiftL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftL64# a1 a2 = GHC.Prim.uncheckedIShiftL64# a1 a2
{-# NOINLINE uncheckedIShiftRA64# #-}
uncheckedIShiftRA64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRA64# a1 a2 = GHC.Prim.uncheckedIShiftRA64# a1 a2
{-# NOINLINE uncheckedIShiftRL64# #-}
uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRL64# a1 a2 = GHC.Prim.uncheckedIShiftRL64# a1 a2
{-# NOINLINE int64ToWord64# #-}
int64ToWord64# :: Int64# -> Word64#
int64ToWord64# a1 = GHC.Prim.int64ToWord64# a1
{-# NOINLINE eqInt64# #-}
eqInt64# :: Int64# -> Int64# -> Int#
eqInt64# a1 a2 = GHC.Prim.eqInt64# a1 a2
{-# NOINLINE geInt64# #-}
geInt64# :: Int64# -> Int64# -> Int#
geInt64# a1 a2 = GHC.Prim.geInt64# a1 a2
{-# NOINLINE gtInt64# #-}
gtInt64# :: Int64# -> Int64# -> Int#
gtInt64# a1 a2 = GHC.Prim.gtInt64# a1 a2
{-# NOINLINE leInt64# #-}
leInt64# :: Int64# -> Int64# -> Int#
leInt64# a1 a2 = GHC.Prim.leInt64# a1 a2
{-# NOINLINE ltInt64# #-}
ltInt64# :: Int64# -> Int64# -> Int#
ltInt64# a1 a2 = GHC.Prim.ltInt64# a1 a2
{-# NOINLINE neInt64# #-}
neInt64# :: Int64# -> Int64# -> Int#
neInt64# a1 a2 = GHC.Prim.neInt64# a1 a2
{-# NOINLINE word64ToWord# #-}
word64ToWord# :: Word64# -> Word#
word64ToWord# a1 = GHC.Prim.word64ToWord# a1
{-# NOINLINE wordToWord64# #-}
wordToWord64# :: Word# -> Word64#
wordToWord64# a1 = GHC.Prim.wordToWord64# a1
{-# NOINLINE plusWord64# #-}
plusWord64# :: Word64# -> Word64# -> Word64#
plusWord64# a1 a2 = GHC.Prim.plusWord64# a1 a2
{-# NOINLINE subWord64# #-}
subWord64# :: Word64# -> Word64# -> Word64#
subWord64# a1 a2 = GHC.Prim.subWord64# a1 a2
{-# NOINLINE timesWord64# #-}
timesWord64# :: Word64# -> Word64# -> Word64#
timesWord64# a1 a2 = GHC.Prim.timesWord64# a1 a2
{-# NOINLINE quotWord64# #-}
quotWord64# :: Word64# -> Word64# -> Word64#
quotWord64# a1 a2 = GHC.Prim.quotWord64# a1 a2
{-# NOINLINE remWord64# #-}
remWord64# :: Word64# -> Word64# -> Word64#
remWord64# a1 a2 = GHC.Prim.remWord64# a1 a2
{-# NOINLINE and64# #-}
and64# :: Word64# -> Word64# -> Word64#
and64# a1 a2 = GHC.Prim.and64# a1 a2
{-# NOINLINE or64# #-}
or64# :: Word64# -> Word64# -> Word64#
or64# a1 a2 = GHC.Prim.or64# a1 a2
{-# NOINLINE xor64# #-}
xor64# :: Word64# -> Word64# -> Word64#
xor64# a1 a2 = GHC.Prim.xor64# a1 a2
{-# NOINLINE not64# #-}
not64# :: Word64# -> Word64#
not64# a1 = GHC.Prim.not64# a1
{-# NOINLINE uncheckedShiftL64# #-}
uncheckedShiftL64# :: Word64# -> Int# -> Word64#
uncheckedShiftL64# a1 a2 = GHC.Prim.uncheckedShiftL64# a1 a2
{-# NOINLINE uncheckedShiftRL64# #-}
uncheckedShiftRL64# :: Word64# -> Int# -> Word64#
uncheckedShiftRL64# a1 a2 = GHC.Prim.uncheckedShiftRL64# a1 a2
{-# NOINLINE word64ToInt64# #-}
word64ToInt64# :: Word64# -> Int64#
word64ToInt64# a1 = GHC.Prim.word64ToInt64# a1
{-# NOINLINE eqWord64# #-}
eqWord64# :: Word64# -> Word64# -> Int#
eqWord64# a1 a2 = GHC.Prim.eqWord64# a1 a2
{-# NOINLINE geWord64# #-}
geWord64# :: Word64# -> Word64# -> Int#
geWord64# a1 a2 = GHC.Prim.geWord64# a1 a2
{-# NOINLINE gtWord64# #-}
gtWord64# :: Word64# -> Word64# -> Int#
gtWord64# a1 a2 = GHC.Prim.gtWord64# a1 a2
{-# NOINLINE leWord64# #-}
leWord64# :: Word64# -> Word64# -> Int#
leWord64# a1 a2 = GHC.Prim.leWord64# a1 a2
{-# NOINLINE ltWord64# #-}
ltWord64# :: Word64# -> Word64# -> Int#
ltWord64# a1 a2 = GHC.Prim.ltWord64# a1 a2
{-# NOINLINE neWord64# #-}
neWord64# :: Word64# -> Word64# -> Int#
neWord64# a1 a2 = GHC.Prim.neWord64# a1 a2
{-# NOINLINE (+#) #-}
(+#) :: Int# -> Int# -> Int#
(+#) a1 a2 = (GHC.Prim.+#) a1 a2
{-# NOINLINE (-#) #-}
(-#) :: Int# -> Int# -> Int#
(-#) a1 a2 = (GHC.Prim.-#) a1 a2
{-# NOINLINE (*#) #-}
(*#) :: Int# -> Int# -> Int#
(*#) a1 a2 = (GHC.Prim.*#) a1 a2
{-# NOINLINE timesInt2# #-}
timesInt2# :: Int# -> Int# -> (# Int#,Int#,Int# #)
timesInt2# a1 a2 = GHC.Prim.timesInt2# a1 a2
{-# NOINLINE mulIntMayOflo# #-}
mulIntMayOflo# :: Int# -> Int# -> Int#
mulIntMayOflo# a1 a2 = GHC.Prim.mulIntMayOflo# a1 a2
{-# NOINLINE quotInt# #-}
quotInt# :: Int# -> Int# -> Int#
quotInt# a1 a2 = GHC.Prim.quotInt# a1 a2
{-# NOINLINE remInt# #-}
remInt# :: Int# -> Int# -> Int#
remInt# a1 a2 = GHC.Prim.remInt# a1 a2
{-# NOINLINE quotRemInt# #-}
quotRemInt# :: Int# -> Int# -> (# Int#,Int# #)
quotRemInt# a1 a2 = GHC.Prim.quotRemInt# a1 a2
{-# NOINLINE andI# #-}
andI# :: Int# -> Int# -> Int#
andI# a1 a2 = GHC.Prim.andI# a1 a2
{-# NOINLINE orI# #-}
orI# :: Int# -> Int# -> Int#
orI# a1 a2 = GHC.Prim.orI# a1 a2
{-# NOINLINE xorI# #-}
xorI# :: Int# -> Int# -> Int#
xorI# a1 a2 = GHC.Prim.xorI# a1 a2
{-# NOINLINE notI# #-}
notI# :: Int# -> Int#
notI# a1 = GHC.Prim.notI# a1
{-# NOINLINE negateInt# #-}
negateInt# :: Int# -> Int#
negateInt# a1 = GHC.Prim.negateInt# a1
{-# NOINLINE addIntC# #-}
addIntC# :: Int# -> Int# -> (# Int#,Int# #)
addIntC# a1 a2 = GHC.Prim.addIntC# a1 a2
{-# NOINLINE subIntC# #-}
subIntC# :: Int# -> Int# -> (# Int#,Int# #)
subIntC# a1 a2 = GHC.Prim.subIntC# a1 a2
{-# NOINLINE (>#) #-}
(>#) :: Int# -> Int# -> Int#
(>#) a1 a2 = (GHC.Prim.>#) a1 a2
{-# NOINLINE (>=#) #-}
(>=#) :: Int# -> Int# -> Int#
(>=#) a1 a2 = (GHC.Prim.>=#) a1 a2
{-# NOINLINE (==#) #-}
(==#) :: Int# -> Int# -> Int#
(==#) a1 a2 = (GHC.Prim.==#) a1 a2
{-# NOINLINE (/=#) #-}
(/=#) :: Int# -> Int# -> Int#
(/=#) a1 a2 = (GHC.Prim./=#) a1 a2
{-# NOINLINE (<#) #-}
(<#) :: Int# -> Int# -> Int#
(<#) a1 a2 = (GHC.Prim.<#) a1 a2
{-# NOINLINE (<=#) #-}
(<=#) :: Int# -> Int# -> Int#
(<=#) a1 a2 = (GHC.Prim.<=#) a1 a2
{-# NOINLINE chr# #-}
chr# :: Int# -> Char#
chr# a1 = GHC.Prim.chr# a1
{-# NOINLINE int2Word# #-}
int2Word# :: Int# -> Word#
int2Word# a1 = GHC.Prim.int2Word# a1
{-# NOINLINE int2Float# #-}
int2Float# :: Int# -> Float#
int2Float# a1 = GHC.Prim.int2Float# a1
{-# NOINLINE int2Double# #-}
int2Double# :: Int# -> Double#
int2Double# a1 = GHC.Prim.int2Double# a1
{-# NOINLINE word2Float# #-}
word2Float# :: Word# -> Float#
word2Float# a1 = GHC.Prim.word2Float# a1
{-# NOINLINE word2Double# #-}
word2Double# :: Word# -> Double#
word2Double# a1 = GHC.Prim.word2Double# a1
{-# NOINLINE uncheckedIShiftL# #-}
uncheckedIShiftL# :: Int# -> Int# -> Int#
uncheckedIShiftL# a1 a2 = GHC.Prim.uncheckedIShiftL# a1 a2
{-# NOINLINE uncheckedIShiftRA# #-}
uncheckedIShiftRA# :: Int# -> Int# -> Int#
uncheckedIShiftRA# a1 a2 = GHC.Prim.uncheckedIShiftRA# a1 a2
{-# NOINLINE uncheckedIShiftRL# #-}
uncheckedIShiftRL# :: Int# -> Int# -> Int#
uncheckedIShiftRL# a1 a2 = GHC.Prim.uncheckedIShiftRL# a1 a2
{-# NOINLINE plusWord# #-}
plusWord# :: Word# -> Word# -> Word#
plusWord# a1 a2 = GHC.Prim.plusWord# a1 a2
{-# NOINLINE addWordC# #-}
addWordC# :: Word# -> Word# -> (# Word#,Int# #)
addWordC# a1 a2 = GHC.Prim.addWordC# a1 a2
{-# NOINLINE subWordC# #-}
subWordC# :: Word# -> Word# -> (# Word#,Int# #)
subWordC# a1 a2 = GHC.Prim.subWordC# a1 a2
{-# NOINLINE plusWord2# #-}
plusWord2# :: Word# -> Word# -> (# Word#,Word# #)
plusWord2# a1 a2 = GHC.Prim.plusWord2# a1 a2
{-# NOINLINE minusWord# #-}
minusWord# :: Word# -> Word# -> Word#
minusWord# a1 a2 = GHC.Prim.minusWord# a1 a2
{-# NOINLINE timesWord# #-}
timesWord# :: Word# -> Word# -> Word#
timesWord# a1 a2 = GHC.Prim.timesWord# a1 a2
{-# NOINLINE timesWord2# #-}
timesWord2# :: Word# -> Word# -> (# Word#,Word# #)
timesWord2# a1 a2 = GHC.Prim.timesWord2# a1 a2
{-# NOINLINE quotWord# #-}
quotWord# :: Word# -> Word# -> Word#
quotWord# a1 a2 = GHC.Prim.quotWord# a1 a2
{-# NOINLINE remWord# #-}
remWord# :: Word# -> Word# -> Word#
remWord# a1 a2 = GHC.Prim.remWord# a1 a2
{-# NOINLINE quotRemWord# #-}
quotRemWord# :: Word# -> Word# -> (# Word#,Word# #)
quotRemWord# a1 a2 = GHC.Prim.quotRemWord# a1 a2
{-# NOINLINE quotRemWord2# #-}
quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#,Word# #)
quotRemWord2# a1 a2 a3 = GHC.Prim.quotRemWord2# a1 a2 a3
{-# NOINLINE and# #-}
and# :: Word# -> Word# -> Word#
and# a1 a2 = GHC.Prim.and# a1 a2
{-# NOINLINE or# #-}
or# :: Word# -> Word# -> Word#
or# a1 a2 = GHC.Prim.or# a1 a2
{-# NOINLINE xor# #-}
xor# :: Word# -> Word# -> Word#
xor# a1 a2 = GHC.Prim.xor# a1 a2
{-# NOINLINE not# #-}
not# :: Word# -> Word#
not# a1 = GHC.Prim.not# a1
{-# NOINLINE uncheckedShiftL# #-}
uncheckedShiftL# :: Word# -> Int# -> Word#
uncheckedShiftL# a1 a2 = GHC.Prim.uncheckedShiftL# a1 a2
{-# NOINLINE uncheckedShiftRL# #-}
uncheckedShiftRL# :: Word# -> Int# -> Word#
uncheckedShiftRL# a1 a2 = GHC.Prim.uncheckedShiftRL# a1 a2
{-# NOINLINE word2Int# #-}
word2Int# :: Word# -> Int#
word2Int# a1 = GHC.Prim.word2Int# a1
{-# NOINLINE gtWord# #-}
gtWord# :: Word# -> Word# -> Int#
gtWord# a1 a2 = GHC.Prim.gtWord# a1 a2
{-# NOINLINE geWord# #-}
geWord# :: Word# -> Word# -> Int#
geWord# a1 a2 = GHC.Prim.geWord# a1 a2
{-# NOINLINE eqWord# #-}
eqWord# :: Word# -> Word# -> Int#
eqWord# a1 a2 = GHC.Prim.eqWord# a1 a2
{-# NOINLINE neWord# #-}
neWord# :: Word# -> Word# -> Int#
neWord# a1 a2 = GHC.Prim.neWord# a1 a2
{-# NOINLINE ltWord# #-}
ltWord# :: Word# -> Word# -> Int#
ltWord# a1 a2 = GHC.Prim.ltWord# a1 a2
{-# NOINLINE leWord# #-}
leWord# :: Word# -> Word# -> Int#
leWord# a1 a2 = GHC.Prim.leWord# a1 a2
{-# NOINLINE popCnt8# #-}
popCnt8# :: Word# -> Word#
popCnt8# a1 = GHC.Prim.popCnt8# a1
{-# NOINLINE popCnt16# #-}
popCnt16# :: Word# -> Word#
popCnt16# a1 = GHC.Prim.popCnt16# a1
{-# NOINLINE popCnt32# #-}
popCnt32# :: Word# -> Word#
popCnt32# a1 = GHC.Prim.popCnt32# a1
{-# NOINLINE popCnt64# #-}
popCnt64# :: Word64# -> Word#
popCnt64# a1 = GHC.Prim.popCnt64# a1
{-# NOINLINE popCnt# #-}
popCnt# :: Word# -> Word#
popCnt# a1 = GHC.Prim.popCnt# a1
{-# NOINLINE pdep8# #-}
pdep8# :: Word# -> Word# -> Word#
pdep8# a1 a2 = GHC.Prim.pdep8# a1 a2
{-# NOINLINE pdep16# #-}
pdep16# :: Word# -> Word# -> Word#
pdep16# a1 a2 = GHC.Prim.pdep16# a1 a2
{-# NOINLINE pdep32# #-}
pdep32# :: Word# -> Word# -> Word#
pdep32# a1 a2 = GHC.Prim.pdep32# a1 a2
{-# NOINLINE pdep64# #-}
pdep64# :: Word64# -> Word64# -> Word64#
pdep64# a1 a2 = GHC.Prim.pdep64# a1 a2
{-# NOINLINE pdep# #-}
pdep# :: Word# -> Word# -> Word#
pdep# a1 a2 = GHC.Prim.pdep# a1 a2
{-# NOINLINE pext8# #-}
pext8# :: Word# -> Word# -> Word#
pext8# a1 a2 = GHC.Prim.pext8# a1 a2
{-# NOINLINE pext16# #-}
pext16# :: Word# -> Word# -> Word#
pext16# a1 a2 = GHC.Prim.pext16# a1 a2
{-# NOINLINE pext32# #-}
pext32# :: Word# -> Word# -> Word#
pext32# a1 a2 = GHC.Prim.pext32# a1 a2
{-# NOINLINE pext64# #-}
pext64# :: Word64# -> Word64# -> Word64#
pext64# a1 a2 = GHC.Prim.pext64# a1 a2
{-# NOINLINE pext# #-}
pext# :: Word# -> Word# -> Word#
pext# a1 a2 = GHC.Prim.pext# a1 a2
{-# NOINLINE clz8# #-}
clz8# :: Word# -> Word#
clz8# a1 = GHC.Prim.clz8# a1
{-# NOINLINE clz16# #-}
clz16# :: Word# -> Word#
clz16# a1 = GHC.Prim.clz16# a1
{-# NOINLINE clz32# #-}
clz32# :: Word# -> Word#
clz32# a1 = GHC.Prim.clz32# a1
{-# NOINLINE clz64# #-}
clz64# :: Word64# -> Word#
clz64# a1 = GHC.Prim.clz64# a1
{-# NOINLINE clz# #-}
clz# :: Word# -> Word#
clz# a1 = GHC.Prim.clz# a1
{-# NOINLINE ctz8# #-}
ctz8# :: Word# -> Word#
ctz8# a1 = GHC.Prim.ctz8# a1
{-# NOINLINE ctz16# #-}
ctz16# :: Word# -> Word#
ctz16# a1 = GHC.Prim.ctz16# a1
{-# NOINLINE ctz32# #-}
ctz32# :: Word# -> Word#
ctz32# a1 = GHC.Prim.ctz32# a1
{-# NOINLINE ctz64# #-}
ctz64# :: Word64# -> Word#
ctz64# a1 = GHC.Prim.ctz64# a1
{-# NOINLINE ctz# #-}
ctz# :: Word# -> Word#
ctz# a1 = GHC.Prim.ctz# a1
{-# NOINLINE byteSwap16# #-}
byteSwap16# :: Word# -> Word#
byteSwap16# a1 = GHC.Prim.byteSwap16# a1
{-# NOINLINE byteSwap32# #-}
byteSwap32# :: Word# -> Word#
byteSwap32# a1 = GHC.Prim.byteSwap32# a1
{-# NOINLINE byteSwap64# #-}
byteSwap64# :: Word64# -> Word64#
byteSwap64# a1 = GHC.Prim.byteSwap64# a1
{-# NOINLINE byteSwap# #-}
byteSwap# :: Word# -> Word#
byteSwap# a1 = GHC.Prim.byteSwap# a1
{-# NOINLINE bitReverse8# #-}
bitReverse8# :: Word# -> Word#
bitReverse8# a1 = GHC.Prim.bitReverse8# a1
{-# NOINLINE bitReverse16# #-}
bitReverse16# :: Word# -> Word#
bitReverse16# a1 = GHC.Prim.bitReverse16# a1
{-# NOINLINE bitReverse32# #-}
bitReverse32# :: Word# -> Word#
bitReverse32# a1 = GHC.Prim.bitReverse32# a1
{-# NOINLINE bitReverse64# #-}
bitReverse64# :: Word64# -> Word64#
bitReverse64# a1 = GHC.Prim.bitReverse64# a1
{-# NOINLINE bitReverse# #-}
bitReverse# :: Word# -> Word#
bitReverse# a1 = GHC.Prim.bitReverse# a1
{-# NOINLINE narrow8Int# #-}
narrow8Int# :: Int# -> Int#
narrow8Int# a1 = GHC.Prim.narrow8Int# a1
{-# NOINLINE narrow16Int# #-}
narrow16Int# :: Int# -> Int#
narrow16Int# a1 = GHC.Prim.narrow16Int# a1
{-# NOINLINE narrow32Int# #-}
narrow32Int# :: Int# -> Int#
narrow32Int# a1 = GHC.Prim.narrow32Int# a1
{-# NOINLINE narrow8Word# #-}
narrow8Word# :: Word# -> Word#
narrow8Word# a1 = GHC.Prim.narrow8Word# a1
{-# NOINLINE narrow16Word# #-}
narrow16Word# :: Word# -> Word#
narrow16Word# a1 = GHC.Prim.narrow16Word# a1
{-# NOINLINE narrow32Word# #-}
narrow32Word# :: Word# -> Word#
narrow32Word# a1 = GHC.Prim.narrow32Word# a1
{-# NOINLINE (>##) #-}
(>##) :: Double# -> Double# -> Int#
(>##) a1 a2 = (GHC.Prim.>##) a1 a2
{-# NOINLINE (>=##) #-}
(>=##) :: Double# -> Double# -> Int#
(>=##) a1 a2 = (GHC.Prim.>=##) a1 a2
{-# NOINLINE (==##) #-}
(==##) :: Double# -> Double# -> Int#
(==##) a1 a2 = (GHC.Prim.==##) a1 a2
{-# NOINLINE (/=##) #-}
(/=##) :: Double# -> Double# -> Int#
(/=##) a1 a2 = (GHC.Prim./=##) a1 a2
{-# NOINLINE (<##) #-}
(<##) :: Double# -> Double# -> Int#
(<##) a1 a2 = (GHC.Prim.<##) a1 a2
{-# NOINLINE (<=##) #-}
(<=##) :: Double# -> Double# -> Int#
(<=##) a1 a2 = (GHC.Prim.<=##) a1 a2
{-# NOINLINE (+##) #-}
(+##) :: Double# -> Double# -> Double#
(+##) a1 a2 = (GHC.Prim.+##) a1 a2
{-# NOINLINE (-##) #-}
(-##) :: Double# -> Double# -> Double#
(-##) a1 a2 = (GHC.Prim.-##) a1 a2
{-# NOINLINE (*##) #-}
(*##) :: Double# -> Double# -> Double#
(*##) a1 a2 = (GHC.Prim.*##) a1 a2
{-# NOINLINE (/##) #-}
(/##) :: Double# -> Double# -> Double#
(/##) a1 a2 = (GHC.Prim./##) a1 a2
{-# NOINLINE negateDouble# #-}
negateDouble# :: Double# -> Double#
negateDouble# a1 = GHC.Prim.negateDouble# a1
{-# NOINLINE fabsDouble# #-}
fabsDouble# :: Double# -> Double#
fabsDouble# a1 = GHC.Prim.fabsDouble# a1
{-# NOINLINE double2Int# #-}
double2Int# :: Double# -> Int#
double2Int# a1 = GHC.Prim.double2Int# a1
{-# NOINLINE double2Float# #-}
double2Float# :: Double# -> Float#
double2Float# a1 = GHC.Prim.double2Float# a1
{-# NOINLINE expDouble# #-}
expDouble# :: Double# -> Double#
expDouble# a1 = GHC.Prim.expDouble# a1
{-# NOINLINE expm1Double# #-}
expm1Double# :: Double# -> Double#
expm1Double# a1 = GHC.Prim.expm1Double# a1
{-# NOINLINE logDouble# #-}
logDouble# :: Double# -> Double#
logDouble# a1 = GHC.Prim.logDouble# a1
{-# NOINLINE log1pDouble# #-}
log1pDouble# :: Double# -> Double#
log1pDouble# a1 = GHC.Prim.log1pDouble# a1
{-# NOINLINE sqrtDouble# #-}
sqrtDouble# :: Double# -> Double#
sqrtDouble# a1 = GHC.Prim.sqrtDouble# a1
{-# NOINLINE sinDouble# #-}
sinDouble# :: Double# -> Double#
sinDouble# a1 = GHC.Prim.sinDouble# a1
{-# NOINLINE cosDouble# #-}
cosDouble# :: Double# -> Double#
cosDouble# a1 = GHC.Prim.cosDouble# a1
{-# NOINLINE tanDouble# #-}
tanDouble# :: Double# -> Double#
tanDouble# a1 = GHC.Prim.tanDouble# a1
{-# NOINLINE asinDouble# #-}
asinDouble# :: Double# -> Double#
asinDouble# a1 = GHC.Prim.asinDouble# a1
{-# NOINLINE acosDouble# #-}
acosDouble# :: Double# -> Double#
acosDouble# a1 = GHC.Prim.acosDouble# a1
{-# NOINLINE atanDouble# #-}
atanDouble# :: Double# -> Double#
atanDouble# a1 = GHC.Prim.atanDouble# a1
{-# NOINLINE sinhDouble# #-}
sinhDouble# :: Double# -> Double#
sinhDouble# a1 = GHC.Prim.sinhDouble# a1
{-# NOINLINE coshDouble# #-}
coshDouble# :: Double# -> Double#
coshDouble# a1 = GHC.Prim.coshDouble# a1
{-# NOINLINE tanhDouble# #-}
tanhDouble# :: Double# -> Double#
tanhDouble# a1 = GHC.Prim.tanhDouble# a1
{-# NOINLINE asinhDouble# #-}
asinhDouble# :: Double# -> Double#
asinhDouble# a1 = GHC.Prim.asinhDouble# a1
{-# NOINLINE acoshDouble# #-}
acoshDouble# :: Double# -> Double#
acoshDouble# a1 = GHC.Prim.acoshDouble# a1
{-# NOINLINE atanhDouble# #-}
atanhDouble# :: Double# -> Double#
atanhDouble# a1 = GHC.Prim.atanhDouble# a1
{-# NOINLINE (**##) #-}
(**##) :: Double# -> Double# -> Double#
(**##) a1 a2 = (GHC.Prim.**##) a1 a2
{-# NOINLINE decodeDouble_2Int# #-}
decodeDouble_2Int# :: Double# -> (# Int#,Word#,Word#,Int# #)
decodeDouble_2Int# a1 = GHC.Prim.decodeDouble_2Int# a1
{-# NOINLINE decodeDouble_Int64# #-}
decodeDouble_Int64# :: Double# -> (# Int64#,Int# #)
decodeDouble_Int64# a1 = GHC.Prim.decodeDouble_Int64# a1
{-# NOINLINE gtFloat# #-}
gtFloat# :: Float# -> Float# -> Int#
gtFloat# a1 a2 = GHC.Prim.gtFloat# a1 a2
{-# NOINLINE geFloat# #-}
geFloat# :: Float# -> Float# -> Int#
geFloat# a1 a2 = GHC.Prim.geFloat# a1 a2
{-# NOINLINE eqFloat# #-}
eqFloat# :: Float# -> Float# -> Int#
eqFloat# a1 a2 = GHC.Prim.eqFloat# a1 a2
{-# NOINLINE neFloat# #-}
neFloat# :: Float# -> Float# -> Int#
neFloat# a1 a2 = GHC.Prim.neFloat# a1 a2
{-# NOINLINE ltFloat# #-}
ltFloat# :: Float# -> Float# -> Int#
ltFloat# a1 a2 = GHC.Prim.ltFloat# a1 a2
{-# NOINLINE leFloat# #-}
leFloat# :: Float# -> Float# -> Int#
leFloat# a1 a2 = GHC.Prim.leFloat# a1 a2
{-# NOINLINE plusFloat# #-}
plusFloat# :: Float# -> Float# -> Float#
plusFloat# a1 a2 = GHC.Prim.plusFloat# a1 a2
{-# NOINLINE minusFloat# #-}
minusFloat# :: Float# -> Float# -> Float#
minusFloat# a1 a2 = GHC.Prim.minusFloat# a1 a2
{-# NOINLINE timesFloat# #-}
timesFloat# :: Float# -> Float# -> Float#
timesFloat# a1 a2 = GHC.Prim.timesFloat# a1 a2
{-# NOINLINE divideFloat# #-}
divideFloat# :: Float# -> Float# -> Float#
divideFloat# a1 a2 = GHC.Prim.divideFloat# a1 a2
{-# NOINLINE negateFloat# #-}
negateFloat# :: Float# -> Float#
negateFloat# a1 = GHC.Prim.negateFloat# a1
{-# NOINLINE fabsFloat# #-}
fabsFloat# :: Float# -> Float#
fabsFloat# a1 = GHC.Prim.fabsFloat# a1
{-# NOINLINE float2Int# #-}
float2Int# :: Float# -> Int#
float2Int# a1 = GHC.Prim.float2Int# a1
{-# NOINLINE expFloat# #-}
expFloat# :: Float# -> Float#
expFloat# a1 = GHC.Prim.expFloat# a1
{-# NOINLINE expm1Float# #-}
expm1Float# :: Float# -> Float#
expm1Float# a1 = GHC.Prim.expm1Float# a1
{-# NOINLINE logFloat# #-}
logFloat# :: Float# -> Float#
logFloat# a1 = GHC.Prim.logFloat# a1
{-# NOINLINE log1pFloat# #-}
log1pFloat# :: Float# -> Float#
log1pFloat# a1 = GHC.Prim.log1pFloat# a1
{-# NOINLINE sqrtFloat# #-}
sqrtFloat# :: Float# -> Float#
sqrtFloat# a1 = GHC.Prim.sqrtFloat# a1
{-# NOINLINE sinFloat# #-}
sinFloat# :: Float# -> Float#
sinFloat# a1 = GHC.Prim.sinFloat# a1
{-# NOINLINE cosFloat# #-}
cosFloat# :: Float# -> Float#
cosFloat# a1 = GHC.Prim.cosFloat# a1
{-# NOINLINE tanFloat# #-}
tanFloat# :: Float# -> Float#
tanFloat# a1 = GHC.Prim.tanFloat# a1
{-# NOINLINE asinFloat# #-}
asinFloat# :: Float# -> Float#
asinFloat# a1 = GHC.Prim.asinFloat# a1
{-# NOINLINE acosFloat# #-}
acosFloat# :: Float# -> Float#
acosFloat# a1 = GHC.Prim.acosFloat# a1
{-# NOINLINE atanFloat# #-}
atanFloat# :: Float# -> Float#
atanFloat# a1 = GHC.Prim.atanFloat# a1
{-# NOINLINE sinhFloat# #-}
sinhFloat# :: Float# -> Float#
sinhFloat# a1 = GHC.Prim.sinhFloat# a1
{-# NOINLINE coshFloat# #-}
coshFloat# :: Float# -> Float#
coshFloat# a1 = GHC.Prim.coshFloat# a1
{-# NOINLINE tanhFloat# #-}
tanhFloat# :: Float# -> Float#
tanhFloat# a1 = GHC.Prim.tanhFloat# a1
{-# NOINLINE asinhFloat# #-}
asinhFloat# :: Float# -> Float#
asinhFloat# a1 = GHC.Prim.asinhFloat# a1
{-# NOINLINE acoshFloat# #-}
acoshFloat# :: Float# -> Float#
acoshFloat# a1 = GHC.Prim.acoshFloat# a1
{-# NOINLINE atanhFloat# #-}
atanhFloat# :: Float# -> Float#
atanhFloat# a1 = GHC.Prim.atanhFloat# a1
{-# NOINLINE powerFloat# #-}
powerFloat# :: Float# -> Float# -> Float#
powerFloat# a1 a2 = GHC.Prim.powerFloat# a1 a2
{-# NOINLINE float2Double# #-}
float2Double# :: Float# -> Double#
float2Double# a1 = GHC.Prim.float2Double# a1
{-# NOINLINE decodeFloat_Int# #-}
decodeFloat_Int# :: Float# -> (# Int#,Int# #)
decodeFloat_Int# a1 = GHC.Prim.decodeFloat_Int# a1
{-# NOINLINE fmaddFloat# #-}
fmaddFloat# :: Float# -> Float# -> Float# -> Float#
fmaddFloat# a1 a2 a3 = GHC.Prim.fmaddFloat# a1 a2 a3
{-# NOINLINE fmsubFloat# #-}
fmsubFloat# :: Float# -> Float# -> Float# -> Float#
fmsubFloat# a1 a2 a3 = GHC.Prim.fmsubFloat# a1 a2 a3
{-# NOINLINE fnmaddFloat# #-}
fnmaddFloat# :: Float# -> Float# -> Float# -> Float#
fnmaddFloat# a1 a2 a3 = GHC.Prim.fnmaddFloat# a1 a2 a3
{-# NOINLINE fnmsubFloat# #-}
fnmsubFloat# :: Float# -> Float# -> Float# -> Float#
fnmsubFloat# a1 a2 a3 = GHC.Prim.fnmsubFloat# a1 a2 a3
{-# NOINLINE fmaddDouble# #-}
fmaddDouble# :: Double# -> Double# -> Double# -> Double#
fmaddDouble# a1 a2 a3 = GHC.Prim.fmaddDouble# a1 a2 a3
{-# NOINLINE fmsubDouble# #-}
fmsubDouble# :: Double# -> Double# -> Double# -> Double#
fmsubDouble# a1 a2 a3 = GHC.Prim.fmsubDouble# a1 a2 a3
{-# NOINLINE fnmaddDouble# #-}
fnmaddDouble# :: Double# -> Double# -> Double# -> Double#
fnmaddDouble# a1 a2 a3 = GHC.Prim.fnmaddDouble# a1 a2 a3
{-# NOINLINE fnmsubDouble# #-}
fnmsubDouble# :: Double# -> Double# -> Double# -> Double#
fnmsubDouble# a1 a2 a3 = GHC.Prim.fnmsubDouble# a1 a2 a3
{-# NOINLINE newArray# #-}
newArray# :: Int# -> v -> State# s -> (# State# s,MutableArray# s v #)
newArray# a1 a2 a3 = GHC.Prim.newArray# a1 a2 a3
{-# NOINLINE readArray# #-}
readArray# :: MutableArray# s v -> Int# -> State# s -> (# State# s,v #)
readArray# a1 a2 a3 = GHC.Prim.readArray# a1 a2 a3
{-# NOINLINE writeArray# #-}
writeArray# :: MutableArray# s v -> Int# -> v -> State# s -> State# s
writeArray# a1 a2 a3 a4 = GHC.Prim.writeArray# a1 a2 a3 a4
{-# NOINLINE sizeofArray# #-}
sizeofArray# :: Array# v -> Int#
sizeofArray# a1 = GHC.Prim.sizeofArray# a1
{-# NOINLINE sizeofMutableArray# #-}
sizeofMutableArray# :: MutableArray# s v -> Int#
sizeofMutableArray# a1 = GHC.Prim.sizeofMutableArray# a1
{-# NOINLINE indexArray# #-}
indexArray# :: Array# v -> Int# -> (# v #)
indexArray# a1 a2 = GHC.Prim.indexArray# a1 a2
{-# NOINLINE unsafeFreezeArray# #-}
unsafeFreezeArray# :: MutableArray# s v -> State# s -> (# State# s,Array# v #)
unsafeFreezeArray# a1 a2 = GHC.Prim.unsafeFreezeArray# a1 a2
{-# NOINLINE unsafeThawArray# #-}
unsafeThawArray# :: Array# v -> State# s -> (# State# s,MutableArray# s v #)
unsafeThawArray# a1 a2 = GHC.Prim.unsafeThawArray# a1 a2
{-# NOINLINE copyArray# #-}
copyArray# :: Array# v -> Int# -> MutableArray# s v -> Int# -> Int# -> State# s -> State# s
copyArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copyArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableArray# #-}
copyMutableArray# :: MutableArray# s v -> Int# -> MutableArray# s v -> Int# -> Int# -> State# s -> State# s
copyMutableArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copyMutableArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE cloneArray# #-}
cloneArray# :: Array# v -> Int# -> Int# -> Array# v
cloneArray# a1 a2 a3 = GHC.Prim.cloneArray# a1 a2 a3
{-# NOINLINE cloneMutableArray# #-}
cloneMutableArray# :: MutableArray# s v -> Int# -> Int# -> State# s -> (# State# s,MutableArray# s v #)
cloneMutableArray# a1 a2 a3 a4 = GHC.Prim.cloneMutableArray# a1 a2 a3 a4
{-# NOINLINE freezeArray# #-}
freezeArray# :: MutableArray# s v -> Int# -> Int# -> State# s -> (# State# s,Array# v #)
freezeArray# a1 a2 a3 a4 = GHC.Prim.freezeArray# a1 a2 a3 a4
{-# NOINLINE thawArray# #-}
thawArray# :: Array# v -> Int# -> Int# -> State# s -> (# State# s,MutableArray# s v #)
thawArray# a1 a2 a3 a4 = GHC.Prim.thawArray# a1 a2 a3 a4
{-# NOINLINE casArray# #-}
casArray# :: MutableArray# s v -> Int# -> v -> v -> State# s -> (# State# s,Int#,v #)
casArray# a1 a2 a3 a4 a5 = GHC.Prim.casArray# a1 a2 a3 a4 a5
{-# NOINLINE newSmallArray# #-}
newSmallArray# :: Int# -> v -> State# s -> (# State# s,SmallMutableArray# s v #)
newSmallArray# a1 a2 a3 = GHC.Prim.newSmallArray# a1 a2 a3
{-# NOINLINE shrinkSmallMutableArray# #-}
shrinkSmallMutableArray# :: SmallMutableArray# s v -> Int# -> State# s -> State# s
shrinkSmallMutableArray# a1 a2 a3 = GHC.Prim.shrinkSmallMutableArray# a1 a2 a3
{-# NOINLINE readSmallArray# #-}
readSmallArray# :: SmallMutableArray# s v -> Int# -> State# s -> (# State# s,v #)
readSmallArray# a1 a2 a3 = GHC.Prim.readSmallArray# a1 a2 a3
{-# NOINLINE writeSmallArray# #-}
writeSmallArray# :: SmallMutableArray# s v -> Int# -> v -> State# s -> State# s
writeSmallArray# a1 a2 a3 a4 = GHC.Prim.writeSmallArray# a1 a2 a3 a4
{-# NOINLINE sizeofSmallArray# #-}
sizeofSmallArray# :: SmallArray# v -> Int#
sizeofSmallArray# a1 = GHC.Prim.sizeofSmallArray# a1
{-# NOINLINE sizeofSmallMutableArray# #-}
sizeofSmallMutableArray# :: SmallMutableArray# s v -> Int#
sizeofSmallMutableArray# a1 = GHC.Prim.sizeofSmallMutableArray# a1
{-# NOINLINE getSizeofSmallMutableArray# #-}
getSizeofSmallMutableArray# :: SmallMutableArray# s v -> State# s -> (# State# s,Int# #)
getSizeofSmallMutableArray# a1 a2 = GHC.Prim.getSizeofSmallMutableArray# a1 a2
{-# NOINLINE indexSmallArray# #-}
indexSmallArray# :: SmallArray# v -> Int# -> (# v #)
indexSmallArray# a1 a2 = GHC.Prim.indexSmallArray# a1 a2
{-# NOINLINE unsafeFreezeSmallArray# #-}
unsafeFreezeSmallArray# :: SmallMutableArray# s v -> State# s -> (# State# s,SmallArray# v #)
unsafeFreezeSmallArray# a1 a2 = GHC.Prim.unsafeFreezeSmallArray# a1 a2
{-# NOINLINE unsafeThawSmallArray# #-}
unsafeThawSmallArray# :: SmallArray# v -> State# s -> (# State# s,SmallMutableArray# s v #)
unsafeThawSmallArray# a1 a2 = GHC.Prim.unsafeThawSmallArray# a1 a2
{-# NOINLINE copySmallArray# #-}
copySmallArray# :: SmallArray# v -> Int# -> SmallMutableArray# s v -> Int# -> Int# -> State# s -> State# s
copySmallArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copySmallArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE copySmallMutableArray# #-}
copySmallMutableArray# :: SmallMutableArray# s v -> Int# -> SmallMutableArray# s v -> Int# -> Int# -> State# s -> State# s
copySmallMutableArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copySmallMutableArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE cloneSmallArray# #-}
cloneSmallArray# :: SmallArray# v -> Int# -> Int# -> SmallArray# v
cloneSmallArray# a1 a2 a3 = GHC.Prim.cloneSmallArray# a1 a2 a3
{-# NOINLINE cloneSmallMutableArray# #-}
cloneSmallMutableArray# :: SmallMutableArray# s v -> Int# -> Int# -> State# s -> (# State# s,SmallMutableArray# s v #)
cloneSmallMutableArray# a1 a2 a3 a4 = GHC.Prim.cloneSmallMutableArray# a1 a2 a3 a4
{-# NOINLINE freezeSmallArray# #-}
freezeSmallArray# :: SmallMutableArray# s v -> Int# -> Int# -> State# s -> (# State# s,SmallArray# v #)
freezeSmallArray# a1 a2 a3 a4 = GHC.Prim.freezeSmallArray# a1 a2 a3 a4
{-# NOINLINE thawSmallArray# #-}
thawSmallArray# :: SmallArray# v -> Int# -> Int# -> State# s -> (# State# s,SmallMutableArray# s v #)
thawSmallArray# a1 a2 a3 a4 = GHC.Prim.thawSmallArray# a1 a2 a3 a4
{-# NOINLINE casSmallArray# #-}
casSmallArray# :: SmallMutableArray# s v -> Int# -> v -> v -> State# s -> (# State# s,Int#,v #)
casSmallArray# a1 a2 a3 a4 a5 = GHC.Prim.casSmallArray# a1 a2 a3 a4 a5
{-# NOINLINE newByteArray# #-}
newByteArray# :: Int# -> State# s -> (# State# s,MutableByteArray# s #)
newByteArray# a1 a2 = GHC.Prim.newByteArray# a1 a2
{-# NOINLINE newPinnedByteArray# #-}
newPinnedByteArray# :: Int# -> State# s -> (# State# s,MutableByteArray# s #)
newPinnedByteArray# a1 a2 = GHC.Prim.newPinnedByteArray# a1 a2
{-# NOINLINE newAlignedPinnedByteArray# #-}
newAlignedPinnedByteArray# :: Int# -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
newAlignedPinnedByteArray# a1 a2 a3 = GHC.Prim.newAlignedPinnedByteArray# a1 a2 a3
{-# NOINLINE isMutableByteArrayPinned# #-}
isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
isMutableByteArrayPinned# a1 = GHC.Prim.isMutableByteArrayPinned# a1
{-# NOINLINE isByteArrayPinned# #-}
isByteArrayPinned# :: ByteArray# -> Int#
isByteArrayPinned# a1 = GHC.Prim.isByteArrayPinned# a1
{-# NOINLINE byteArrayContents# #-}
byteArrayContents# :: ByteArray# -> Addr#
byteArrayContents# a1 = GHC.Prim.byteArrayContents# a1
{-# NOINLINE mutableByteArrayContents# #-}
mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# a1 = GHC.Prim.mutableByteArrayContents# a1
{-# NOINLINE shrinkMutableByteArray# #-}
shrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> State# s
shrinkMutableByteArray# a1 a2 a3 = GHC.Prim.shrinkMutableByteArray# a1 a2 a3
{-# NOINLINE resizeMutableByteArray# #-}
resizeMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
resizeMutableByteArray# a1 a2 a3 = GHC.Prim.resizeMutableByteArray# a1 a2 a3
{-# NOINLINE unsafeFreezeByteArray# #-}
unsafeFreezeByteArray# :: MutableByteArray# s -> State# s -> (# State# s,ByteArray# #)
unsafeFreezeByteArray# a1 a2 = GHC.Prim.unsafeFreezeByteArray# a1 a2
{-# NOINLINE sizeofByteArray# #-}
sizeofByteArray# :: ByteArray# -> Int#
sizeofByteArray# a1 = GHC.Prim.sizeofByteArray# a1
{-# NOINLINE sizeofMutableByteArray# #-}
sizeofMutableByteArray# :: MutableByteArray# s -> Int#
sizeofMutableByteArray# a1 = GHC.Prim.sizeofMutableByteArray# a1
{-# NOINLINE getSizeofMutableByteArray# #-}
getSizeofMutableByteArray# :: MutableByteArray# s -> State# s -> (# State# s,Int# #)
getSizeofMutableByteArray# a1 a2 = GHC.Prim.getSizeofMutableByteArray# a1 a2
{-# NOINLINE indexCharArray# #-}
indexCharArray# :: ByteArray# -> Int# -> Char#
indexCharArray# a1 a2 = GHC.Prim.indexCharArray# a1 a2
{-# NOINLINE indexWideCharArray# #-}
indexWideCharArray# :: ByteArray# -> Int# -> Char#
indexWideCharArray# a1 a2 = GHC.Prim.indexWideCharArray# a1 a2
{-# NOINLINE indexIntArray# #-}
indexIntArray# :: ByteArray# -> Int# -> Int#
indexIntArray# a1 a2 = GHC.Prim.indexIntArray# a1 a2
{-# NOINLINE indexWordArray# #-}
indexWordArray# :: ByteArray# -> Int# -> Word#
indexWordArray# a1 a2 = GHC.Prim.indexWordArray# a1 a2
{-# NOINLINE indexAddrArray# #-}
indexAddrArray# :: ByteArray# -> Int# -> Addr#
indexAddrArray# a1 a2 = GHC.Prim.indexAddrArray# a1 a2
{-# NOINLINE indexFloatArray# #-}
indexFloatArray# :: ByteArray# -> Int# -> Float#
indexFloatArray# a1 a2 = GHC.Prim.indexFloatArray# a1 a2
{-# NOINLINE indexDoubleArray# #-}
indexDoubleArray# :: ByteArray# -> Int# -> Double#
indexDoubleArray# a1 a2 = GHC.Prim.indexDoubleArray# a1 a2
{-# NOINLINE indexStablePtrArray# #-}
indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a
indexStablePtrArray# a1 a2 = GHC.Prim.indexStablePtrArray# a1 a2
{-# NOINLINE indexInt8Array# #-}
indexInt8Array# :: ByteArray# -> Int# -> Int8#
indexInt8Array# a1 a2 = GHC.Prim.indexInt8Array# a1 a2
{-# NOINLINE indexWord8Array# #-}
indexWord8Array# :: ByteArray# -> Int# -> Word8#
indexWord8Array# a1 a2 = GHC.Prim.indexWord8Array# a1 a2
{-# NOINLINE indexInt16Array# #-}
indexInt16Array# :: ByteArray# -> Int# -> Int16#
indexInt16Array# a1 a2 = GHC.Prim.indexInt16Array# a1 a2
{-# NOINLINE indexWord16Array# #-}
indexWord16Array# :: ByteArray# -> Int# -> Word16#
indexWord16Array# a1 a2 = GHC.Prim.indexWord16Array# a1 a2
{-# NOINLINE indexInt32Array# #-}
indexInt32Array# :: ByteArray# -> Int# -> Int32#
indexInt32Array# a1 a2 = GHC.Prim.indexInt32Array# a1 a2
{-# NOINLINE indexWord32Array# #-}
indexWord32Array# :: ByteArray# -> Int# -> Word32#
indexWord32Array# a1 a2 = GHC.Prim.indexWord32Array# a1 a2
{-# NOINLINE indexInt64Array# #-}
indexInt64Array# :: ByteArray# -> Int# -> Int64#
indexInt64Array# a1 a2 = GHC.Prim.indexInt64Array# a1 a2
{-# NOINLINE indexWord64Array# #-}
indexWord64Array# :: ByteArray# -> Int# -> Word64#
indexWord64Array# a1 a2 = GHC.Prim.indexWord64Array# a1 a2
{-# NOINLINE indexWord8ArrayAsChar# #-}
indexWord8ArrayAsChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsChar# a1 a2 = GHC.Prim.indexWord8ArrayAsChar# a1 a2
{-# NOINLINE indexWord8ArrayAsWideChar# #-}
indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#
indexWord8ArrayAsWideChar# a1 a2 = GHC.Prim.indexWord8ArrayAsWideChar# a1 a2
{-# NOINLINE indexWord8ArrayAsInt# #-}
indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#
indexWord8ArrayAsInt# a1 a2 = GHC.Prim.indexWord8ArrayAsInt# a1 a2
{-# NOINLINE indexWord8ArrayAsWord# #-}
indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord# a1 a2 = GHC.Prim.indexWord8ArrayAsWord# a1 a2
{-# NOINLINE indexWord8ArrayAsAddr# #-}
indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#
indexWord8ArrayAsAddr# a1 a2 = GHC.Prim.indexWord8ArrayAsAddr# a1 a2
{-# NOINLINE indexWord8ArrayAsFloat# #-}
indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#
indexWord8ArrayAsFloat# a1 a2 = GHC.Prim.indexWord8ArrayAsFloat# a1 a2
{-# NOINLINE indexWord8ArrayAsDouble# #-}
indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#
indexWord8ArrayAsDouble# a1 a2 = GHC.Prim.indexWord8ArrayAsDouble# a1 a2
{-# NOINLINE indexWord8ArrayAsStablePtr# #-}
indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a
indexWord8ArrayAsStablePtr# a1 a2 = GHC.Prim.indexWord8ArrayAsStablePtr# a1 a2
{-# NOINLINE indexWord8ArrayAsInt16# #-}
indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int16#
indexWord8ArrayAsInt16# a1 a2 = GHC.Prim.indexWord8ArrayAsInt16# a1 a2
{-# NOINLINE indexWord8ArrayAsWord16# #-}
indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word16#
indexWord8ArrayAsWord16# a1 a2 = GHC.Prim.indexWord8ArrayAsWord16# a1 a2
{-# NOINLINE indexWord8ArrayAsInt32# #-}
indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> Int32#
indexWord8ArrayAsInt32# a1 a2 = GHC.Prim.indexWord8ArrayAsInt32# a1 a2
{-# NOINLINE indexWord8ArrayAsWord32# #-}
indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word32#
indexWord8ArrayAsWord32# a1 a2 = GHC.Prim.indexWord8ArrayAsWord32# a1 a2
{-# NOINLINE indexWord8ArrayAsInt64# #-}
indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> Int64#
indexWord8ArrayAsInt64# a1 a2 = GHC.Prim.indexWord8ArrayAsInt64# a1 a2
{-# NOINLINE indexWord8ArrayAsWord64# #-}
indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word64#
indexWord8ArrayAsWord64# a1 a2 = GHC.Prim.indexWord8ArrayAsWord64# a1 a2
{-# NOINLINE readCharArray# #-}
readCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readCharArray# a1 a2 a3 = GHC.Prim.readCharArray# a1 a2 a3
{-# NOINLINE readWideCharArray# #-}
readWideCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readWideCharArray# a1 a2 a3 = GHC.Prim.readWideCharArray# a1 a2 a3
{-# NOINLINE readIntArray# #-}
readIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readIntArray# a1 a2 a3 = GHC.Prim.readIntArray# a1 a2 a3
{-# NOINLINE readWordArray# #-}
readWordArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWordArray# a1 a2 a3 = GHC.Prim.readWordArray# a1 a2 a3
{-# NOINLINE readAddrArray# #-}
readAddrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Addr# #)
readAddrArray# a1 a2 a3 = GHC.Prim.readAddrArray# a1 a2 a3
{-# NOINLINE readFloatArray# #-}
readFloatArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Float# #)
readFloatArray# a1 a2 a3 = GHC.Prim.readFloatArray# a1 a2 a3
{-# NOINLINE readDoubleArray# #-}
readDoubleArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Double# #)
readDoubleArray# a1 a2 a3 = GHC.Prim.readDoubleArray# a1 a2 a3
{-# NOINLINE readStablePtrArray# #-}
readStablePtrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,StablePtr# a #)
readStablePtrArray# a1 a2 a3 = GHC.Prim.readStablePtrArray# a1 a2 a3
{-# NOINLINE readInt8Array# #-}
readInt8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int8# #)
readInt8Array# a1 a2 a3 = GHC.Prim.readInt8Array# a1 a2 a3
{-# NOINLINE readWord8Array# #-}
readWord8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word8# #)
readWord8Array# a1 a2 a3 = GHC.Prim.readWord8Array# a1 a2 a3
{-# NOINLINE readInt16Array# #-}
readInt16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int16# #)
readInt16Array# a1 a2 a3 = GHC.Prim.readInt16Array# a1 a2 a3
{-# NOINLINE readWord16Array# #-}
readWord16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word16# #)
readWord16Array# a1 a2 a3 = GHC.Prim.readWord16Array# a1 a2 a3
{-# NOINLINE readInt32Array# #-}
readInt32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int32# #)
readInt32Array# a1 a2 a3 = GHC.Prim.readInt32Array# a1 a2 a3
{-# NOINLINE readWord32Array# #-}
readWord32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word32# #)
readWord32Array# a1 a2 a3 = GHC.Prim.readWord32Array# a1 a2 a3
{-# NOINLINE readInt64Array# #-}
readInt64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int64# #)
readInt64Array# a1 a2 a3 = GHC.Prim.readInt64Array# a1 a2 a3
{-# NOINLINE readWord64Array# #-}
readWord64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word64# #)
readWord64Array# a1 a2 a3 = GHC.Prim.readWord64Array# a1 a2 a3
{-# NOINLINE readWord8ArrayAsChar# #-}
readWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readWord8ArrayAsChar# a1 a2 a3 = GHC.Prim.readWord8ArrayAsChar# a1 a2 a3
{-# NOINLINE readWord8ArrayAsWideChar# #-}
readWord8ArrayAsWideChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Char# #)
readWord8ArrayAsWideChar# a1 a2 a3 = GHC.Prim.readWord8ArrayAsWideChar# a1 a2 a3
{-# NOINLINE readWord8ArrayAsInt# #-}
readWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
readWord8ArrayAsInt# a1 a2 a3 = GHC.Prim.readWord8ArrayAsInt# a1 a2 a3
{-# NOINLINE readWord8ArrayAsWord# #-}
readWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word# #)
readWord8ArrayAsWord# a1 a2 a3 = GHC.Prim.readWord8ArrayAsWord# a1 a2 a3
{-# NOINLINE readWord8ArrayAsAddr# #-}
readWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Addr# #)
readWord8ArrayAsAddr# a1 a2 a3 = GHC.Prim.readWord8ArrayAsAddr# a1 a2 a3
{-# NOINLINE readWord8ArrayAsFloat# #-}
readWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Float# #)
readWord8ArrayAsFloat# a1 a2 a3 = GHC.Prim.readWord8ArrayAsFloat# a1 a2 a3
{-# NOINLINE readWord8ArrayAsDouble# #-}
readWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Double# #)
readWord8ArrayAsDouble# a1 a2 a3 = GHC.Prim.readWord8ArrayAsDouble# a1 a2 a3
{-# NOINLINE readWord8ArrayAsStablePtr# #-}
readWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,StablePtr# a #)
readWord8ArrayAsStablePtr# a1 a2 a3 = GHC.Prim.readWord8ArrayAsStablePtr# a1 a2 a3
{-# NOINLINE readWord8ArrayAsInt16# #-}
readWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int16# #)
readWord8ArrayAsInt16# a1 a2 a3 = GHC.Prim.readWord8ArrayAsInt16# a1 a2 a3
{-# NOINLINE readWord8ArrayAsWord16# #-}
readWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word16# #)
readWord8ArrayAsWord16# a1 a2 a3 = GHC.Prim.readWord8ArrayAsWord16# a1 a2 a3
{-# NOINLINE readWord8ArrayAsInt32# #-}
readWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int32# #)
readWord8ArrayAsInt32# a1 a2 a3 = GHC.Prim.readWord8ArrayAsInt32# a1 a2 a3
{-# NOINLINE readWord8ArrayAsWord32# #-}
readWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word32# #)
readWord8ArrayAsWord32# a1 a2 a3 = GHC.Prim.readWord8ArrayAsWord32# a1 a2 a3
{-# NOINLINE readWord8ArrayAsInt64# #-}
readWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int64# #)
readWord8ArrayAsInt64# a1 a2 a3 = GHC.Prim.readWord8ArrayAsInt64# a1 a2 a3
{-# NOINLINE readWord8ArrayAsWord64# #-}
readWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Word64# #)
readWord8ArrayAsWord64# a1 a2 a3 = GHC.Prim.readWord8ArrayAsWord64# a1 a2 a3
{-# NOINLINE writeCharArray# #-}
writeCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeCharArray# a1 a2 a3 a4 = GHC.Prim.writeCharArray# a1 a2 a3 a4
{-# NOINLINE writeWideCharArray# #-}
writeWideCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeWideCharArray# a1 a2 a3 a4 = GHC.Prim.writeWideCharArray# a1 a2 a3 a4
{-# NOINLINE writeIntArray# #-}
writeIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeIntArray# a1 a2 a3 a4 = GHC.Prim.writeIntArray# a1 a2 a3 a4
{-# NOINLINE writeWordArray# #-}
writeWordArray# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWordArray# a1 a2 a3 a4 = GHC.Prim.writeWordArray# a1 a2 a3 a4
{-# NOINLINE writeAddrArray# #-}
writeAddrArray# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
writeAddrArray# a1 a2 a3 a4 = GHC.Prim.writeAddrArray# a1 a2 a3 a4
{-# NOINLINE writeFloatArray# #-}
writeFloatArray# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
writeFloatArray# a1 a2 a3 a4 = GHC.Prim.writeFloatArray# a1 a2 a3 a4
{-# NOINLINE writeDoubleArray# #-}
writeDoubleArray# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
writeDoubleArray# a1 a2 a3 a4 = GHC.Prim.writeDoubleArray# a1 a2 a3 a4
{-# NOINLINE writeStablePtrArray# #-}
writeStablePtrArray# :: MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
writeStablePtrArray# a1 a2 a3 a4 = GHC.Prim.writeStablePtrArray# a1 a2 a3 a4
{-# NOINLINE writeInt8Array# #-}
writeInt8Array# :: MutableByteArray# s -> Int# -> Int8# -> State# s -> State# s
writeInt8Array# a1 a2 a3 a4 = GHC.Prim.writeInt8Array# a1 a2 a3 a4
{-# NOINLINE writeWord8Array# #-}
writeWord8Array# :: MutableByteArray# s -> Int# -> Word8# -> State# s -> State# s
writeWord8Array# a1 a2 a3 a4 = GHC.Prim.writeWord8Array# a1 a2 a3 a4
{-# NOINLINE writeInt16Array# #-}
writeInt16Array# :: MutableByteArray# s -> Int# -> Int16# -> State# s -> State# s
writeInt16Array# a1 a2 a3 a4 = GHC.Prim.writeInt16Array# a1 a2 a3 a4
{-# NOINLINE writeWord16Array# #-}
writeWord16Array# :: MutableByteArray# s -> Int# -> Word16# -> State# s -> State# s
writeWord16Array# a1 a2 a3 a4 = GHC.Prim.writeWord16Array# a1 a2 a3 a4
{-# NOINLINE writeInt32Array# #-}
writeInt32Array# :: MutableByteArray# s -> Int# -> Int32# -> State# s -> State# s
writeInt32Array# a1 a2 a3 a4 = GHC.Prim.writeInt32Array# a1 a2 a3 a4
{-# NOINLINE writeWord32Array# #-}
writeWord32Array# :: MutableByteArray# s -> Int# -> Word32# -> State# s -> State# s
writeWord32Array# a1 a2 a3 a4 = GHC.Prim.writeWord32Array# a1 a2 a3 a4
{-# NOINLINE writeInt64Array# #-}
writeInt64Array# :: MutableByteArray# s -> Int# -> Int64# -> State# s -> State# s
writeInt64Array# a1 a2 a3 a4 = GHC.Prim.writeInt64Array# a1 a2 a3 a4
{-# NOINLINE writeWord64Array# #-}
writeWord64Array# :: MutableByteArray# s -> Int# -> Word64# -> State# s -> State# s
writeWord64Array# a1 a2 a3 a4 = GHC.Prim.writeWord64Array# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsChar# #-}
writeWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeWord8ArrayAsChar# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsChar# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsWideChar# #-}
writeWord8ArrayAsWideChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
writeWord8ArrayAsWideChar# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsWideChar# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsInt# #-}
writeWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
writeWord8ArrayAsInt# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsInt# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsWord# #-}
writeWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
writeWord8ArrayAsWord# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsWord# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsAddr# #-}
writeWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
writeWord8ArrayAsAddr# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsAddr# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsFloat# #-}
writeWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
writeWord8ArrayAsFloat# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsFloat# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsDouble# #-}
writeWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
writeWord8ArrayAsDouble# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsDouble# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsStablePtr# #-}
writeWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
writeWord8ArrayAsStablePtr# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsStablePtr# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsInt16# #-}
writeWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> Int16# -> State# s -> State# s
writeWord8ArrayAsInt16# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsInt16# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsWord16# #-}
writeWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> Word16# -> State# s -> State# s
writeWord8ArrayAsWord16# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsWord16# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsInt32# #-}
writeWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> Int32# -> State# s -> State# s
writeWord8ArrayAsInt32# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsInt32# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsWord32# #-}
writeWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> Word32# -> State# s -> State# s
writeWord8ArrayAsWord32# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsWord32# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsInt64# #-}
writeWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> Int64# -> State# s -> State# s
writeWord8ArrayAsInt64# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsInt64# a1 a2 a3 a4
{-# NOINLINE writeWord8ArrayAsWord64# #-}
writeWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> Word64# -> State# s -> State# s
writeWord8ArrayAsWord64# a1 a2 a3 a4 = GHC.Prim.writeWord8ArrayAsWord64# a1 a2 a3 a4
{-# NOINLINE compareByteArrays# #-}
compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
compareByteArrays# a1 a2 a3 a4 a5 = GHC.Prim.compareByteArrays# a1 a2 a3 a4 a5
{-# NOINLINE copyByteArray# #-}
copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyByteArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copyByteArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableByteArray# #-}
copyMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableByteArray# a1 a2 a3 a4 a5 a6 = GHC.Prim.copyMutableByteArray# a1 a2 a3 a4 a5 a6
{-# NOINLINE copyMutableByteArrayNonOverlapping# #-}
copyMutableByteArrayNonOverlapping# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyMutableByteArrayNonOverlapping# a1 a2 a3 a4 a5 a6 = GHC.Prim.copyMutableByteArrayNonOverlapping# a1 a2 a3 a4 a5 a6
{-# NOINLINE copyByteArrayToAddr# #-}
copyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
copyByteArrayToAddr# a1 a2 a3 a4 a5 = GHC.Prim.copyByteArrayToAddr# a1 a2 a3 a4 a5
{-# NOINLINE copyMutableByteArrayToAddr# #-}
copyMutableByteArrayToAddr# :: MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
copyMutableByteArrayToAddr# a1 a2 a3 a4 a5 = GHC.Prim.copyMutableByteArrayToAddr# a1 a2 a3 a4 a5
{-# NOINLINE copyAddrToByteArray# #-}
copyAddrToByteArray# :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyAddrToByteArray# a1 a2 a3 a4 a5 = GHC.Prim.copyAddrToByteArray# a1 a2 a3 a4 a5
{-# NOINLINE copyAddrToAddr# #-}
copyAddrToAddr# :: Addr# -> Addr# -> Int# -> State# (RealWorld) -> State# (RealWorld)
copyAddrToAddr# a1 a2 a3 a4 = GHC.Prim.copyAddrToAddr# a1 a2 a3 a4
{-# NOINLINE copyAddrToAddrNonOverlapping# #-}
copyAddrToAddrNonOverlapping# :: Addr# -> Addr# -> Int# -> State# (RealWorld) -> State# (RealWorld)
copyAddrToAddrNonOverlapping# a1 a2 a3 a4 = GHC.Prim.copyAddrToAddrNonOverlapping# a1 a2 a3 a4
{-# NOINLINE setByteArray# #-}
setByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
setByteArray# a1 a2 a3 a4 a5 = GHC.Prim.setByteArray# a1 a2 a3 a4 a5
{-# NOINLINE setAddrRange# #-}
setAddrRange# :: Addr# -> Int# -> Int# -> State# (RealWorld) -> State# (RealWorld)
setAddrRange# a1 a2 a3 a4 = GHC.Prim.setAddrRange# a1 a2 a3 a4
{-# NOINLINE atomicReadIntArray# #-}
atomicReadIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,Int# #)
atomicReadIntArray# a1 a2 a3 = GHC.Prim.atomicReadIntArray# a1 a2 a3
{-# NOINLINE atomicWriteIntArray# #-}
atomicWriteIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
atomicWriteIntArray# a1 a2 a3 a4 = GHC.Prim.atomicWriteIntArray# a1 a2 a3 a4
{-# NOINLINE casIntArray# #-}
casIntArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s,Int# #)
casIntArray# a1 a2 a3 a4 a5 = GHC.Prim.casIntArray# a1 a2 a3 a4 a5
{-# NOINLINE casInt8Array# #-}
casInt8Array# :: MutableByteArray# s -> Int# -> Int8# -> Int8# -> State# s -> (# State# s,Int8# #)
casInt8Array# a1 a2 a3 a4 a5 = GHC.Prim.casInt8Array# a1 a2 a3 a4 a5
{-# NOINLINE casInt16Array# #-}
casInt16Array# :: MutableByteArray# s -> Int# -> Int16# -> Int16# -> State# s -> (# State# s,Int16# #)
casInt16Array# a1 a2 a3 a4 a5 = GHC.Prim.casInt16Array# a1 a2 a3 a4 a5
{-# NOINLINE casInt32Array# #-}
casInt32Array# :: MutableByteArray# s -> Int# -> Int32# -> Int32# -> State# s -> (# State# s,Int32# #)
casInt32Array# a1 a2 a3 a4 a5 = GHC.Prim.casInt32Array# a1 a2 a3 a4 a5
{-# NOINLINE casInt64Array# #-}
casInt64Array# :: MutableByteArray# s -> Int# -> Int64# -> Int64# -> State# s -> (# State# s,Int64# #)
casInt64Array# a1 a2 a3 a4 a5 = GHC.Prim.casInt64Array# a1 a2 a3 a4 a5
{-# NOINLINE fetchAddIntArray# #-}
fetchAddIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchAddIntArray# a1 a2 a3 a4 = GHC.Prim.fetchAddIntArray# a1 a2 a3 a4
{-# NOINLINE fetchSubIntArray# #-}
fetchSubIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchSubIntArray# a1 a2 a3 a4 = GHC.Prim.fetchSubIntArray# a1 a2 a3 a4
{-# NOINLINE fetchAndIntArray# #-}
fetchAndIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchAndIntArray# a1 a2 a3 a4 = GHC.Prim.fetchAndIntArray# a1 a2 a3 a4
{-# NOINLINE fetchNandIntArray# #-}
fetchNandIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchNandIntArray# a1 a2 a3 a4 = GHC.Prim.fetchNandIntArray# a1 a2 a3 a4
{-# NOINLINE fetchOrIntArray# #-}
fetchOrIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchOrIntArray# a1 a2 a3 a4 = GHC.Prim.fetchOrIntArray# a1 a2 a3 a4
{-# NOINLINE fetchXorIntArray# #-}
fetchXorIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s,Int# #)
fetchXorIntArray# a1 a2 a3 a4 = GHC.Prim.fetchXorIntArray# a1 a2 a3 a4
{-# NOINLINE plusAddr# #-}
plusAddr# :: Addr# -> Int# -> Addr#
plusAddr# a1 a2 = GHC.Prim.plusAddr# a1 a2
{-# NOINLINE minusAddr# #-}
minusAddr# :: Addr# -> Addr# -> Int#
minusAddr# a1 a2 = GHC.Prim.minusAddr# a1 a2
{-# NOINLINE remAddr# #-}
remAddr# :: Addr# -> Int# -> Int#
remAddr# a1 a2 = GHC.Prim.remAddr# a1 a2
{-# NOINLINE addr2Int# #-}
addr2Int# :: Addr# -> Int#
addr2Int# a1 = GHC.Prim.addr2Int# a1
{-# NOINLINE int2Addr# #-}
int2Addr# :: Int# -> Addr#
int2Addr# a1 = GHC.Prim.int2Addr# a1
{-# NOINLINE gtAddr# #-}
gtAddr# :: Addr# -> Addr# -> Int#
gtAddr# a1 a2 = GHC.Prim.gtAddr# a1 a2
{-# NOINLINE geAddr# #-}
geAddr# :: Addr# -> Addr# -> Int#
geAddr# a1 a2 = GHC.Prim.geAddr# a1 a2
{-# NOINLINE eqAddr# #-}
eqAddr# :: Addr# -> Addr# -> Int#
eqAddr# a1 a2 = GHC.Prim.eqAddr# a1 a2
{-# NOINLINE neAddr# #-}
neAddr# :: Addr# -> Addr# -> Int#
neAddr# a1 a2 = GHC.Prim.neAddr# a1 a2
{-# NOINLINE ltAddr# #-}
ltAddr# :: Addr# -> Addr# -> Int#
ltAddr# a1 a2 = GHC.Prim.ltAddr# a1 a2
{-# NOINLINE leAddr# #-}
leAddr# :: Addr# -> Addr# -> Int#
leAddr# a1 a2 = GHC.Prim.leAddr# a1 a2
{-# NOINLINE indexCharOffAddr# #-}
indexCharOffAddr# :: Addr# -> Int# -> Char#
indexCharOffAddr# a1 a2 = GHC.Prim.indexCharOffAddr# a1 a2
{-# NOINLINE indexWideCharOffAddr# #-}
indexWideCharOffAddr# :: Addr# -> Int# -> Char#
indexWideCharOffAddr# a1 a2 = GHC.Prim.indexWideCharOffAddr# a1 a2
{-# NOINLINE indexIntOffAddr# #-}
indexIntOffAddr# :: Addr# -> Int# -> Int#
indexIntOffAddr# a1 a2 = GHC.Prim.indexIntOffAddr# a1 a2
{-# NOINLINE indexWordOffAddr# #-}
indexWordOffAddr# :: Addr# -> Int# -> Word#
indexWordOffAddr# a1 a2 = GHC.Prim.indexWordOffAddr# a1 a2
{-# NOINLINE indexAddrOffAddr# #-}
indexAddrOffAddr# :: Addr# -> Int# -> Addr#
indexAddrOffAddr# a1 a2 = GHC.Prim.indexAddrOffAddr# a1 a2
{-# NOINLINE indexFloatOffAddr# #-}
indexFloatOffAddr# :: Addr# -> Int# -> Float#
indexFloatOffAddr# a1 a2 = GHC.Prim.indexFloatOffAddr# a1 a2
{-# NOINLINE indexDoubleOffAddr# #-}
indexDoubleOffAddr# :: Addr# -> Int# -> Double#
indexDoubleOffAddr# a1 a2 = GHC.Prim.indexDoubleOffAddr# a1 a2
{-# NOINLINE indexStablePtrOffAddr# #-}
indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
indexStablePtrOffAddr# a1 a2 = GHC.Prim.indexStablePtrOffAddr# a1 a2
{-# NOINLINE indexInt8OffAddr# #-}
indexInt8OffAddr# :: Addr# -> Int# -> Int8#
indexInt8OffAddr# a1 a2 = GHC.Prim.indexInt8OffAddr# a1 a2
{-# NOINLINE indexWord8OffAddr# #-}
indexWord8OffAddr# :: Addr# -> Int# -> Word8#
indexWord8OffAddr# a1 a2 = GHC.Prim.indexWord8OffAddr# a1 a2
{-# NOINLINE indexInt16OffAddr# #-}
indexInt16OffAddr# :: Addr# -> Int# -> Int16#
indexInt16OffAddr# a1 a2 = GHC.Prim.indexInt16OffAddr# a1 a2
{-# NOINLINE indexWord16OffAddr# #-}
indexWord16OffAddr# :: Addr# -> Int# -> Word16#
indexWord16OffAddr# a1 a2 = GHC.Prim.indexWord16OffAddr# a1 a2
{-# NOINLINE indexInt32OffAddr# #-}
indexInt32OffAddr# :: Addr# -> Int# -> Int32#
indexInt32OffAddr# a1 a2 = GHC.Prim.indexInt32OffAddr# a1 a2
{-# NOINLINE indexWord32OffAddr# #-}
indexWord32OffAddr# :: Addr# -> Int# -> Word32#
indexWord32OffAddr# a1 a2 = GHC.Prim.indexWord32OffAddr# a1 a2
{-# NOINLINE indexInt64OffAddr# #-}
indexInt64OffAddr# :: Addr# -> Int# -> Int64#
indexInt64OffAddr# a1 a2 = GHC.Prim.indexInt64OffAddr# a1 a2
{-# NOINLINE indexWord64OffAddr# #-}
indexWord64OffAddr# :: Addr# -> Int# -> Word64#
indexWord64OffAddr# a1 a2 = GHC.Prim.indexWord64OffAddr# a1 a2
{-# NOINLINE readCharOffAddr# #-}
readCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)
readCharOffAddr# a1 a2 a3 = GHC.Prim.readCharOffAddr# a1 a2 a3
{-# NOINLINE readWideCharOffAddr# #-}
readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Char# #)
readWideCharOffAddr# a1 a2 a3 = GHC.Prim.readWideCharOffAddr# a1 a2 a3
{-# NOINLINE readIntOffAddr# #-}
readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int# #)
readIntOffAddr# a1 a2 a3 = GHC.Prim.readIntOffAddr# a1 a2 a3
{-# NOINLINE readWordOffAddr# #-}
readWordOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word# #)
readWordOffAddr# a1 a2 a3 = GHC.Prim.readWordOffAddr# a1 a2 a3
{-# NOINLINE readAddrOffAddr# #-}
readAddrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Addr# #)
readAddrOffAddr# a1 a2 a3 = GHC.Prim.readAddrOffAddr# a1 a2 a3
{-# NOINLINE readFloatOffAddr# #-}
readFloatOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Float# #)
readFloatOffAddr# a1 a2 a3 = GHC.Prim.readFloatOffAddr# a1 a2 a3
{-# NOINLINE readDoubleOffAddr# #-}
readDoubleOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Double# #)
readDoubleOffAddr# a1 a2 a3 = GHC.Prim.readDoubleOffAddr# a1 a2 a3
{-# NOINLINE readStablePtrOffAddr# #-}
readStablePtrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s,StablePtr# a #)
readStablePtrOffAddr# a1 a2 a3 = GHC.Prim.readStablePtrOffAddr# a1 a2 a3
{-# NOINLINE readInt8OffAddr# #-}
readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int8# #)
readInt8OffAddr# a1 a2 a3 = GHC.Prim.readInt8OffAddr# a1 a2 a3
{-# NOINLINE readWord8OffAddr# #-}
readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word8# #)
readWord8OffAddr# a1 a2 a3 = GHC.Prim.readWord8OffAddr# a1 a2 a3
{-# NOINLINE readInt16OffAddr# #-}
readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int16# #)
readInt16OffAddr# a1 a2 a3 = GHC.Prim.readInt16OffAddr# a1 a2 a3
{-# NOINLINE readWord16OffAddr# #-}
readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word16# #)
readWord16OffAddr# a1 a2 a3 = GHC.Prim.readWord16OffAddr# a1 a2 a3
{-# NOINLINE readInt32OffAddr# #-}
readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int32# #)
readInt32OffAddr# a1 a2 a3 = GHC.Prim.readInt32OffAddr# a1 a2 a3
{-# NOINLINE readWord32OffAddr# #-}
readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word32# #)
readWord32OffAddr# a1 a2 a3 = GHC.Prim.readWord32OffAddr# a1 a2 a3
{-# NOINLINE readInt64OffAddr# #-}
readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Int64# #)
readInt64OffAddr# a1 a2 a3 = GHC.Prim.readInt64OffAddr# a1 a2 a3
{-# NOINLINE readWord64OffAddr# #-}
readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s,Word64# #)
readWord64OffAddr# a1 a2 a3 = GHC.Prim.readWord64OffAddr# a1 a2 a3
{-# NOINLINE writeCharOffAddr# #-}
writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeCharOffAddr# a1 a2 a3 a4 = GHC.Prim.writeCharOffAddr# a1 a2 a3 a4
{-# NOINLINE writeWideCharOffAddr# #-}
writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
writeWideCharOffAddr# a1 a2 a3 a4 = GHC.Prim.writeWideCharOffAddr# a1 a2 a3 a4
{-# NOINLINE writeIntOffAddr# #-}
writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
writeIntOffAddr# a1 a2 a3 a4 = GHC.Prim.writeIntOffAddr# a1 a2 a3 a4
{-# NOINLINE writeWordOffAddr# #-}
writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
writeWordOffAddr# a1 a2 a3 a4 = GHC.Prim.writeWordOffAddr# a1 a2 a3 a4
{-# NOINLINE writeAddrOffAddr# #-}
writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
writeAddrOffAddr# a1 a2 a3 a4 = GHC.Prim.writeAddrOffAddr# a1 a2 a3 a4
{-# NOINLINE writeFloatOffAddr# #-}
writeFloatOffAddr# :: Addr# -> Int# -> Float# -> State# s -> State# s
writeFloatOffAddr# a1 a2 a3 a4 = GHC.Prim.writeFloatOffAddr# a1 a2 a3 a4
{-# NOINLINE writeDoubleOffAddr# #-}
writeDoubleOffAddr# :: Addr# -> Int# -> Double# -> State# s -> State# s
writeDoubleOffAddr# a1 a2 a3 a4 = GHC.Prim.writeDoubleOffAddr# a1 a2 a3 a4
{-# NOINLINE writeStablePtrOffAddr# #-}
writeStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a -> State# s -> State# s
writeStablePtrOffAddr# a1 a2 a3 a4 = GHC.Prim.writeStablePtrOffAddr# a1 a2 a3 a4
{-# NOINLINE writeInt8OffAddr# #-}
writeInt8OffAddr# :: Addr# -> Int# -> Int8# -> State# s -> State# s
writeInt8OffAddr# a1 a2 a3 a4 = GHC.Prim.writeInt8OffAddr# a1 a2 a3 a4
{-# NOINLINE writeWord8OffAddr# #-}
writeWord8OffAddr# :: Addr# -> Int# -> Word8# -> State# s -> State# s
writeWord8OffAddr# a1 a2 a3 a4 = GHC.Prim.writeWord8OffAddr# a1 a2 a3 a4
{-# NOINLINE writeInt16OffAddr# #-}
writeInt16OffAddr# :: Addr# -> Int# -> Int16# -> State# s -> State# s
writeInt16OffAddr# a1 a2 a3 a4 = GHC.Prim.writeInt16OffAddr# a1 a2 a3 a4
{-# NOINLINE writeWord16OffAddr# #-}
writeWord16OffAddr# :: Addr# -> Int# -> Word16# -> State# s -> State# s
writeWord16OffAddr# a1 a2 a3 a4 = GHC.Prim.writeWord16OffAddr# a1 a2 a3 a4
{-# NOINLINE writeInt32OffAddr# #-}
writeInt32OffAddr# :: Addr# -> Int# -> Int32# -> State# s -> State# s
writeInt32OffAddr# a1 a2 a3 a4 = GHC.Prim.writeInt32OffAddr# a1 a2 a3 a4
{-# NOINLINE writeWord32OffAddr# #-}
writeWord32OffAddr# :: Addr# -> Int# -> Word32# -> State# s -> State# s
writeWord32OffAddr# a1 a2 a3 a4 = GHC.Prim.writeWord32OffAddr# a1 a2 a3 a4
{-# NOINLINE writeInt64OffAddr# #-}
writeInt64OffAddr# :: Addr# -> Int# -> Int64# -> State# s -> State# s
writeInt64OffAddr# a1 a2 a3 a4 = GHC.Prim.writeInt64OffAddr# a1 a2 a3 a4
{-# NOINLINE writeWord64OffAddr# #-}
writeWord64OffAddr# :: Addr# -> Int# -> Word64# -> State# s -> State# s
writeWord64OffAddr# a1 a2 a3 a4 = GHC.Prim.writeWord64OffAddr# a1 a2 a3 a4
{-# NOINLINE atomicExchangeAddrAddr# #-}
atomicExchangeAddrAddr# :: Addr# -> Addr# -> State# s -> (# State# s,Addr# #)
atomicExchangeAddrAddr# a1 a2 a3 = GHC.Prim.atomicExchangeAddrAddr# a1 a2 a3
{-# NOINLINE atomicExchangeWordAddr# #-}
atomicExchangeWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
atomicExchangeWordAddr# a1 a2 a3 = GHC.Prim.atomicExchangeWordAddr# a1 a2 a3
{-# NOINLINE atomicCasAddrAddr# #-}
atomicCasAddrAddr# :: Addr# -> Addr# -> Addr# -> State# s -> (# State# s,Addr# #)
atomicCasAddrAddr# a1 a2 a3 a4 = GHC.Prim.atomicCasAddrAddr# a1 a2 a3 a4
{-# NOINLINE atomicCasWordAddr# #-}
atomicCasWordAddr# :: Addr# -> Word# -> Word# -> State# s -> (# State# s,Word# #)
atomicCasWordAddr# a1 a2 a3 a4 = GHC.Prim.atomicCasWordAddr# a1 a2 a3 a4
{-# NOINLINE atomicCasWord8Addr# #-}
atomicCasWord8Addr# :: Addr# -> Word8# -> Word8# -> State# s -> (# State# s,Word8# #)
atomicCasWord8Addr# a1 a2 a3 a4 = GHC.Prim.atomicCasWord8Addr# a1 a2 a3 a4
{-# NOINLINE atomicCasWord16Addr# #-}
atomicCasWord16Addr# :: Addr# -> Word16# -> Word16# -> State# s -> (# State# s,Word16# #)
atomicCasWord16Addr# a1 a2 a3 a4 = GHC.Prim.atomicCasWord16Addr# a1 a2 a3 a4
{-# NOINLINE atomicCasWord32Addr# #-}
atomicCasWord32Addr# :: Addr# -> Word32# -> Word32# -> State# s -> (# State# s,Word32# #)
atomicCasWord32Addr# a1 a2 a3 a4 = GHC.Prim.atomicCasWord32Addr# a1 a2 a3 a4
{-# NOINLINE atomicCasWord64Addr# #-}
atomicCasWord64Addr# :: Addr# -> Word64# -> Word64# -> State# s -> (# State# s,Word64# #)
atomicCasWord64Addr# a1 a2 a3 a4 = GHC.Prim.atomicCasWord64Addr# a1 a2 a3 a4
{-# NOINLINE fetchAddWordAddr# #-}
fetchAddWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchAddWordAddr# a1 a2 a3 = GHC.Prim.fetchAddWordAddr# a1 a2 a3
{-# NOINLINE fetchSubWordAddr# #-}
fetchSubWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchSubWordAddr# a1 a2 a3 = GHC.Prim.fetchSubWordAddr# a1 a2 a3
{-# NOINLINE fetchAndWordAddr# #-}
fetchAndWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchAndWordAddr# a1 a2 a3 = GHC.Prim.fetchAndWordAddr# a1 a2 a3
{-# NOINLINE fetchNandWordAddr# #-}
fetchNandWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchNandWordAddr# a1 a2 a3 = GHC.Prim.fetchNandWordAddr# a1 a2 a3
{-# NOINLINE fetchOrWordAddr# #-}
fetchOrWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchOrWordAddr# a1 a2 a3 = GHC.Prim.fetchOrWordAddr# a1 a2 a3
{-# NOINLINE fetchXorWordAddr# #-}
fetchXorWordAddr# :: Addr# -> Word# -> State# s -> (# State# s,Word# #)
fetchXorWordAddr# a1 a2 a3 = GHC.Prim.fetchXorWordAddr# a1 a2 a3
{-# NOINLINE atomicReadWordAddr# #-}
atomicReadWordAddr# :: Addr# -> State# s -> (# State# s,Word# #)
atomicReadWordAddr# a1 a2 = GHC.Prim.atomicReadWordAddr# a1 a2
{-# NOINLINE atomicWriteWordAddr# #-}
atomicWriteWordAddr# :: Addr# -> Word# -> State# s -> State# s
atomicWriteWordAddr# a1 a2 a3 = GHC.Prim.atomicWriteWordAddr# a1 a2 a3
{-# NOINLINE newMutVar# #-}
newMutVar# :: v -> State# s -> (# State# s,MutVar# s v #)
newMutVar# a1 a2 = GHC.Prim.newMutVar# a1 a2
{-# NOINLINE readMutVar# #-}
readMutVar# :: MutVar# s v -> State# s -> (# State# s,v #)
readMutVar# a1 a2 = GHC.Prim.readMutVar# a1 a2
{-# NOINLINE writeMutVar# #-}
writeMutVar# :: MutVar# s v -> v -> State# s -> State# s
writeMutVar# a1 a2 a3 = GHC.Prim.writeMutVar# a1 a2 a3
{-# NOINLINE atomicSwapMutVar# #-}
atomicSwapMutVar# :: MutVar# s v -> v -> State# s -> (# State# s,v #)
atomicSwapMutVar# a1 a2 a3 = GHC.Prim.atomicSwapMutVar# a1 a2 a3
{-# NOINLINE atomicModifyMutVar2# #-}
atomicModifyMutVar2# :: MutVar# s a -> (a -> c) -> State# s -> (# State# s,a,c #)
atomicModifyMutVar2# a1 a2 a3 = GHC.Prim.atomicModifyMutVar2# a1 a2 a3
{-# NOINLINE atomicModifyMutVar_# #-}
atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s,a,a #)
atomicModifyMutVar_# a1 a2 a3 = GHC.Prim.atomicModifyMutVar_# a1 a2 a3
{-# NOINLINE casMutVar# #-}
casMutVar# :: MutVar# s v -> v -> v -> State# s -> (# State# s,Int#,v #)
casMutVar# a1 a2 a3 a4 = GHC.Prim.casMutVar# a1 a2 a3 a4
{-# NOINLINE catch# #-}
catch# :: (State# (RealWorld) -> (# State# (RealWorld),o #)) -> (w -> State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),o #)
catch# a1 a2 a3 = GHC.Prim.catch# a1 a2 a3
{-# NOINLINE raise# #-}
raise# :: v -> p
raise# a1 = GHC.Prim.raise# a1
{-# NOINLINE raiseUnderflow# #-}
raiseUnderflow# :: (#  #) -> p
raiseUnderflow# a1 = GHC.Prim.raiseUnderflow# a1
{-# NOINLINE raiseOverflow# #-}
raiseOverflow# :: (#  #) -> p
raiseOverflow# a1 = GHC.Prim.raiseOverflow# a1
{-# NOINLINE raiseDivZero# #-}
raiseDivZero# :: (#  #) -> p
raiseDivZero# a1 = GHC.Prim.raiseDivZero# a1
{-# NOINLINE raiseIO# #-}
raiseIO# :: v -> State# (RealWorld) -> (# State# (RealWorld),p #)
raiseIO# a1 a2 = GHC.Prim.raiseIO# a1 a2
{-# NOINLINE maskAsyncExceptions# #-}
maskAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),o #)
maskAsyncExceptions# a1 a2 = GHC.Prim.maskAsyncExceptions# a1 a2
{-# NOINLINE maskUninterruptible# #-}
maskUninterruptible# :: (State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),o #)
maskUninterruptible# a1 a2 = GHC.Prim.maskUninterruptible# a1 a2
{-# NOINLINE unmaskAsyncExceptions# #-}
unmaskAsyncExceptions# :: (State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),o #)
unmaskAsyncExceptions# a1 a2 = GHC.Prim.unmaskAsyncExceptions# a1 a2
{-# NOINLINE getMaskingState# #-}
getMaskingState# :: State# (RealWorld) -> (# State# (RealWorld),Int# #)
getMaskingState# a1 = GHC.Prim.getMaskingState# a1
{-# NOINLINE newPromptTag# #-}
newPromptTag# :: State# (RealWorld) -> (# State# (RealWorld),PromptTag# a #)
newPromptTag# a1 = GHC.Prim.newPromptTag# a1
{-# NOINLINE prompt# #-}
prompt# :: PromptTag# a -> (State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)
prompt# a1 a2 a3 = GHC.Prim.prompt# a1 a2 a3
{-# NOINLINE control0# #-}
control0# :: PromptTag# a -> (((State# (RealWorld) -> (# State# (RealWorld),p #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),a #)) -> State# (RealWorld) -> (# State# (RealWorld),p #)
control0# a1 a2 a3 = GHC.Prim.control0# a1 a2 a3
{-# NOINLINE atomically# #-}
atomically# :: (State# (RealWorld) -> (# State# (RealWorld),v #)) -> State# (RealWorld) -> (# State# (RealWorld),v #)
atomically# a1 a2 = GHC.Prim.atomically# a1 a2
{-# NOINLINE retry# #-}
retry# :: State# (RealWorld) -> (# State# (RealWorld),v #)
retry# a1 = GHC.Prim.retry# a1
{-# NOINLINE catchRetry# #-}
catchRetry# :: (State# (RealWorld) -> (# State# (RealWorld),v #)) -> (State# (RealWorld) -> (# State# (RealWorld),v #)) -> State# (RealWorld) -> (# State# (RealWorld),v #)
catchRetry# a1 a2 a3 = GHC.Prim.catchRetry# a1 a2 a3
{-# NOINLINE catchSTM# #-}
catchSTM# :: (State# (RealWorld) -> (# State# (RealWorld),v #)) -> (b -> State# (RealWorld) -> (# State# (RealWorld),v #)) -> State# (RealWorld) -> (# State# (RealWorld),v #)
catchSTM# a1 a2 a3 = GHC.Prim.catchSTM# a1 a2 a3
{-# NOINLINE newTVar# #-}
newTVar# :: v -> State# s -> (# State# s,TVar# s v #)
newTVar# a1 a2 = GHC.Prim.newTVar# a1 a2
{-# NOINLINE readTVar# #-}
readTVar# :: TVar# s v -> State# s -> (# State# s,v #)
readTVar# a1 a2 = GHC.Prim.readTVar# a1 a2
{-# NOINLINE readTVarIO# #-}
readTVarIO# :: TVar# s v -> State# s -> (# State# s,v #)
readTVarIO# a1 a2 = GHC.Prim.readTVarIO# a1 a2
{-# NOINLINE writeTVar# #-}
writeTVar# :: TVar# s v -> v -> State# s -> State# s
writeTVar# a1 a2 a3 = GHC.Prim.writeTVar# a1 a2 a3
{-# NOINLINE newMVar# #-}
newMVar# :: State# s -> (# State# s,MVar# s v #)
newMVar# a1 = GHC.Prim.newMVar# a1
{-# NOINLINE takeMVar# #-}
takeMVar# :: MVar# s v -> State# s -> (# State# s,v #)
takeMVar# a1 a2 = GHC.Prim.takeMVar# a1 a2
{-# NOINLINE tryTakeMVar# #-}
tryTakeMVar# :: MVar# s v -> State# s -> (# State# s,Int#,v #)
tryTakeMVar# a1 a2 = GHC.Prim.tryTakeMVar# a1 a2
{-# NOINLINE putMVar# #-}
putMVar# :: MVar# s v -> v -> State# s -> State# s
putMVar# a1 a2 a3 = GHC.Prim.putMVar# a1 a2 a3
{-# NOINLINE tryPutMVar# #-}
tryPutMVar# :: MVar# s v -> v -> State# s -> (# State# s,Int# #)
tryPutMVar# a1 a2 a3 = GHC.Prim.tryPutMVar# a1 a2 a3
{-# NOINLINE readMVar# #-}
readMVar# :: MVar# s v -> State# s -> (# State# s,v #)
readMVar# a1 a2 = GHC.Prim.readMVar# a1 a2
{-# NOINLINE tryReadMVar# #-}
tryReadMVar# :: MVar# s v -> State# s -> (# State# s,Int#,v #)
tryReadMVar# a1 a2 = GHC.Prim.tryReadMVar# a1 a2
{-# NOINLINE isEmptyMVar# #-}
isEmptyMVar# :: MVar# s v -> State# s -> (# State# s,Int# #)
isEmptyMVar# a1 a2 = GHC.Prim.isEmptyMVar# a1 a2
{-# NOINLINE newIOPort# #-}
newIOPort# :: State# s -> (# State# s,IOPort# s v #)
newIOPort# a1 = GHC.Prim.newIOPort# a1
{-# NOINLINE readIOPort# #-}
readIOPort# :: IOPort# s v -> State# s -> (# State# s,v #)
readIOPort# a1 a2 = GHC.Prim.readIOPort# a1 a2
{-# NOINLINE writeIOPort# #-}
writeIOPort# :: IOPort# s v -> v -> State# s -> (# State# s,Int# #)
writeIOPort# a1 a2 a3 = GHC.Prim.writeIOPort# a1 a2 a3
{-# NOINLINE delay# #-}
delay# :: Int# -> State# s -> State# s
delay# a1 a2 = GHC.Prim.delay# a1 a2
{-# NOINLINE waitRead# #-}
waitRead# :: Int# -> State# s -> State# s
waitRead# a1 a2 = GHC.Prim.waitRead# a1 a2
{-# NOINLINE waitWrite# #-}
waitWrite# :: Int# -> State# s -> State# s
waitWrite# a1 a2 = GHC.Prim.waitWrite# a1 a2
{-# NOINLINE fork# #-}
fork# :: (State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
fork# a1 a2 = GHC.Prim.fork# a1 a2
{-# NOINLINE forkOn# #-}
forkOn# :: Int# -> (State# (RealWorld) -> (# State# (RealWorld),o #)) -> State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
forkOn# a1 a2 a3 = GHC.Prim.forkOn# a1 a2 a3
{-# NOINLINE killThread# #-}
killThread# :: ThreadId# -> a -> State# (RealWorld) -> State# (RealWorld)
killThread# a1 a2 a3 = GHC.Prim.killThread# a1 a2 a3
{-# NOINLINE yield# #-}
yield# :: State# (RealWorld) -> State# (RealWorld)
yield# a1 = GHC.Prim.yield# a1
{-# NOINLINE myThreadId# #-}
myThreadId# :: State# (RealWorld) -> (# State# (RealWorld),ThreadId# #)
myThreadId# a1 = GHC.Prim.myThreadId# a1
{-# NOINLINE labelThread# #-}
labelThread# :: ThreadId# -> ByteArray# -> State# (RealWorld) -> State# (RealWorld)
labelThread# a1 a2 a3 = GHC.Prim.labelThread# a1 a2 a3
{-# NOINLINE isCurrentThreadBound# #-}
isCurrentThreadBound# :: State# (RealWorld) -> (# State# (RealWorld),Int# #)
isCurrentThreadBound# a1 = GHC.Prim.isCurrentThreadBound# a1
{-# NOINLINE noDuplicate# #-}
noDuplicate# :: State# s -> State# s
noDuplicate# a1 = GHC.Prim.noDuplicate# a1
{-# NOINLINE threadLabel# #-}
threadLabel# :: ThreadId# -> State# (RealWorld) -> (# State# (RealWorld),Int#,ByteArray# #)
threadLabel# a1 a2 = GHC.Prim.threadLabel# a1 a2
{-# NOINLINE threadStatus# #-}
threadStatus# :: ThreadId# -> State# (RealWorld) -> (# State# (RealWorld),Int#,Int#,Int# #)
threadStatus# a1 a2 = GHC.Prim.threadStatus# a1 a2
{-# NOINLINE listThreads# #-}
listThreads# :: State# (RealWorld) -> (# State# (RealWorld),Array# (ThreadId#) #)
listThreads# a1 = GHC.Prim.listThreads# a1
{-# NOINLINE mkWeak# #-}
mkWeak# :: v -> w -> (State# (RealWorld) -> (# State# (RealWorld),c #)) -> State# (RealWorld) -> (# State# (RealWorld),Weak# w #)
mkWeak# a1 a2 a3 a4 = GHC.Prim.mkWeak# a1 a2 a3 a4
{-# NOINLINE mkWeakNoFinalizer# #-}
mkWeakNoFinalizer# :: v -> w -> State# (RealWorld) -> (# State# (RealWorld),Weak# w #)
mkWeakNoFinalizer# a1 a2 a3 = GHC.Prim.mkWeakNoFinalizer# a1 a2 a3
{-# NOINLINE addCFinalizerToWeak# #-}
addCFinalizerToWeak# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# w -> State# (RealWorld) -> (# State# (RealWorld),Int# #)
addCFinalizerToWeak# a1 a2 a3 a4 a5 a6 = GHC.Prim.addCFinalizerToWeak# a1 a2 a3 a4 a5 a6
{-# NOINLINE deRefWeak# #-}
deRefWeak# :: Weak# v -> State# (RealWorld) -> (# State# (RealWorld),Int#,v #)
deRefWeak# a1 a2 = GHC.Prim.deRefWeak# a1 a2
{-# NOINLINE finalizeWeak# #-}
finalizeWeak# :: Weak# v -> State# (RealWorld) -> (# State# (RealWorld),Int#,State# (RealWorld) -> (# State# (RealWorld),b #) #)
finalizeWeak# a1 a2 = GHC.Prim.finalizeWeak# a1 a2
{-# NOINLINE touch# #-}
touch# :: v -> State# s -> State# s
touch# a1 a2 = GHC.Prim.touch# a1 a2
{-# NOINLINE makeStablePtr# #-}
makeStablePtr# :: v -> State# (RealWorld) -> (# State# (RealWorld),StablePtr# v #)
makeStablePtr# a1 a2 = GHC.Prim.makeStablePtr# a1 a2
{-# NOINLINE deRefStablePtr# #-}
deRefStablePtr# :: StablePtr# v -> State# (RealWorld) -> (# State# (RealWorld),v #)
deRefStablePtr# a1 a2 = GHC.Prim.deRefStablePtr# a1 a2
{-# NOINLINE eqStablePtr# #-}
eqStablePtr# :: StablePtr# v -> StablePtr# v -> Int#
eqStablePtr# a1 a2 = GHC.Prim.eqStablePtr# a1 a2
{-# NOINLINE makeStableName# #-}
makeStableName# :: v -> State# (RealWorld) -> (# State# (RealWorld),StableName# v #)
makeStableName# a1 a2 = GHC.Prim.makeStableName# a1 a2
{-# NOINLINE stableNameToInt# #-}
stableNameToInt# :: StableName# v -> Int#
stableNameToInt# a1 = GHC.Prim.stableNameToInt# a1
{-# NOINLINE compactNew# #-}
compactNew# :: Word# -> State# (RealWorld) -> (# State# (RealWorld),Compact# #)
compactNew# a1 a2 = GHC.Prim.compactNew# a1 a2
{-# NOINLINE compactResize# #-}
compactResize# :: Compact# -> Word# -> State# (RealWorld) -> State# (RealWorld)
compactResize# a1 a2 a3 = GHC.Prim.compactResize# a1 a2 a3
{-# NOINLINE compactContains# #-}
compactContains# :: Compact# -> a -> State# (RealWorld) -> (# State# (RealWorld),Int# #)
compactContains# a1 a2 a3 = GHC.Prim.compactContains# a1 a2 a3
{-# NOINLINE compactContainsAny# #-}
compactContainsAny# :: a -> State# (RealWorld) -> (# State# (RealWorld),Int# #)
compactContainsAny# a1 a2 = GHC.Prim.compactContainsAny# a1 a2
{-# NOINLINE compactGetFirstBlock# #-}
compactGetFirstBlock# :: Compact# -> State# (RealWorld) -> (# State# (RealWorld),Addr#,Word# #)
compactGetFirstBlock# a1 a2 = GHC.Prim.compactGetFirstBlock# a1 a2
{-# NOINLINE compactGetNextBlock# #-}
compactGetNextBlock# :: Compact# -> Addr# -> State# (RealWorld) -> (# State# (RealWorld),Addr#,Word# #)
compactGetNextBlock# a1 a2 a3 = GHC.Prim.compactGetNextBlock# a1 a2 a3
{-# NOINLINE compactAllocateBlock# #-}
compactAllocateBlock# :: Word# -> Addr# -> State# (RealWorld) -> (# State# (RealWorld),Addr# #)
compactAllocateBlock# a1 a2 a3 = GHC.Prim.compactAllocateBlock# a1 a2 a3
{-# NOINLINE compactFixupPointers# #-}
compactFixupPointers# :: Addr# -> Addr# -> State# (RealWorld) -> (# State# (RealWorld),Compact#,Addr# #)
compactFixupPointers# a1 a2 a3 = GHC.Prim.compactFixupPointers# a1 a2 a3
{-# NOINLINE compactAdd# #-}
compactAdd# :: Compact# -> a -> State# (RealWorld) -> (# State# (RealWorld),a #)
compactAdd# a1 a2 a3 = GHC.Prim.compactAdd# a1 a2 a3
{-# NOINLINE compactAddWithSharing# #-}
compactAddWithSharing# :: Compact# -> a -> State# (RealWorld) -> (# State# (RealWorld),a #)
compactAddWithSharing# a1 a2 a3 = GHC.Prim.compactAddWithSharing# a1 a2 a3
{-# NOINLINE compactSize# #-}
compactSize# :: Compact# -> State# (RealWorld) -> (# State# (RealWorld),Word# #)
compactSize# a1 a2 = GHC.Prim.compactSize# a1 a2
{-# NOINLINE reallyUnsafePtrEquality# #-}
reallyUnsafePtrEquality# :: v -> w -> Int#
reallyUnsafePtrEquality# a1 a2 = GHC.Prim.reallyUnsafePtrEquality# a1 a2
{-# NOINLINE par# #-}
par# :: a -> Int#
par# a1 = GHC.Prim.par# a1
{-# NOINLINE spark# #-}
spark# :: a -> State# s -> (# State# s,a #)
spark# a1 a2 = GHC.Prim.spark# a1 a2
{-# NOINLINE seq# #-}
seq# :: a -> State# s -> (# State# s,a #)
seq# a1 a2 = GHC.Prim.seq# a1 a2
{-# NOINLINE getSpark# #-}
getSpark# :: State# s -> (# State# s,Int#,a #)
getSpark# a1 = GHC.Prim.getSpark# a1
{-# NOINLINE numSparks# #-}
numSparks# :: State# s -> (# State# s,Int# #)
numSparks# a1 = GHC.Prim.numSparks# a1
{-# NOINLINE keepAlive# #-}
keepAlive# :: v -> State# s -> (State# s -> p) -> p
keepAlive# a1 a2 a3 = GHC.Prim.keepAlive# a1 a2 a3
{-# NOINLINE dataToTag# #-}
dataToTag# :: a -> Int#
dataToTag# a1 = GHC.Prim.dataToTag# a1
{-# NOINLINE addrToAny# #-}
addrToAny# :: Addr# -> (# v #)
addrToAny# a1 = GHC.Prim.addrToAny# a1
{-# NOINLINE anyToAddr# #-}
anyToAddr# :: a -> State# (RealWorld) -> (# State# (RealWorld),Addr# #)
anyToAddr# a1 a2 = GHC.Prim.anyToAddr# a1 a2
{-# NOINLINE mkApUpd0# #-}
mkApUpd0# :: BCO -> (# a #)
mkApUpd0# a1 = GHC.Prim.mkApUpd0# a1
{-# NOINLINE newBCO# #-}
newBCO# :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s,BCO #)
newBCO# a1 a2 a3 a4 a5 a6 = GHC.Prim.newBCO# a1 a2 a3 a4 a5 a6
{-# NOINLINE unpackClosure# #-}
unpackClosure# :: a -> (# Addr#,ByteArray#,Array# b #)
unpackClosure# a1 = GHC.Prim.unpackClosure# a1
{-# NOINLINE closureSize# #-}
closureSize# :: a -> Int#
closureSize# a1 = GHC.Prim.closureSize# a1
{-# NOINLINE getApStackVal# #-}
getApStackVal# :: a -> Int# -> (# Int#,b #)
getApStackVal# a1 a2 = GHC.Prim.getApStackVal# a1 a2
{-# NOINLINE getCCSOf# #-}
getCCSOf# :: a -> State# s -> (# State# s,Addr# #)
getCCSOf# a1 a2 = GHC.Prim.getCCSOf# a1 a2
{-# NOINLINE getCurrentCCS# #-}
getCurrentCCS# :: a -> State# s -> (# State# s,Addr# #)
getCurrentCCS# a1 a2 = GHC.Prim.getCurrentCCS# a1 a2
{-# NOINLINE clearCCS# #-}
clearCCS# :: (State# s -> (# State# s,a #)) -> State# s -> (# State# s,a #)
clearCCS# a1 a2 = GHC.Prim.clearCCS# a1 a2
{-# NOINLINE whereFrom# #-}
whereFrom# :: a -> State# s -> (# State# s,Addr# #)
whereFrom# a1 a2 = GHC.Prim.whereFrom# a1 a2
{-# NOINLINE traceEvent# #-}
traceEvent# :: Addr# -> State# s -> State# s
traceEvent# a1 a2 = GHC.Prim.traceEvent# a1 a2
{-# NOINLINE traceBinaryEvent# #-}
traceBinaryEvent# :: Addr# -> Int# -> State# s -> State# s
traceBinaryEvent# a1 a2 a3 = GHC.Prim.traceBinaryEvent# a1 a2 a3
{-# NOINLINE traceMarker# #-}
traceMarker# :: Addr# -> State# s -> State# s
traceMarker# a1 a2 = GHC.Prim.traceMarker# a1 a2
{-# NOINLINE setThreadAllocationCounter# #-}
setThreadAllocationCounter# :: Int64# -> State# (RealWorld) -> State# (RealWorld)
setThreadAllocationCounter# a1 a2 = GHC.Prim.setThreadAllocationCounter# a1 a2
{-# NOINLINE prefetchByteArray3# #-}
prefetchByteArray3# :: ByteArray# -> Int# -> State# s -> State# s
prefetchByteArray3# a1 a2 a3 = GHC.Prim.prefetchByteArray3# a1 a2 a3
{-# NOINLINE prefetchMutableByteArray3# #-}
prefetchMutableByteArray3# :: MutableByteArray# s -> Int# -> State# s -> State# s
prefetchMutableByteArray3# a1 a2 a3 = GHC.Prim.prefetchMutableByteArray3# a1 a2 a3
{-# NOINLINE prefetchAddr3# #-}
prefetchAddr3# :: Addr# -> Int# -> State# s -> State# s
prefetchAddr3# a1 a2 a3 = GHC.Prim.prefetchAddr3# a1 a2 a3
{-# NOINLINE prefetchValue3# #-}
prefetchValue3# :: a -> State# s -> State# s
prefetchValue3# a1 a2 = GHC.Prim.prefetchValue3# a1 a2
{-# NOINLINE prefetchByteArray2# #-}
prefetchByteArray2# :: ByteArray# -> Int# -> State# s -> State# s
prefetchByteArray2# a1 a2 a3 = GHC.Prim.prefetchByteArray2# a1 a2 a3
{-# NOINLINE prefetchMutableByteArray2# #-}
prefetchMutableByteArray2# :: MutableByteArray# s -> Int# -> State# s -> State# s
prefetchMutableByteArray2# a1 a2 a3 = GHC.Prim.prefetchMutableByteArray2# a1 a2 a3
{-# NOINLINE prefetchAddr2# #-}
prefetchAddr2# :: Addr# -> Int# -> State# s -> State# s
prefetchAddr2# a1 a2 a3 = GHC.Prim.prefetchAddr2# a1 a2 a3
{-# NOINLINE prefetchValue2# #-}
prefetchValue2# :: a -> State# s -> State# s
prefetchValue2# a1 a2 = GHC.Prim.prefetchValue2# a1 a2
{-# NOINLINE prefetchByteArray1# #-}
prefetchByteArray1# :: ByteArray# -> Int# -> State# s -> State# s
prefetchByteArray1# a1 a2 a3 = GHC.Prim.prefetchByteArray1# a1 a2 a3
{-# NOINLINE prefetchMutableByteArray1# #-}
prefetchMutableByteArray1# :: MutableByteArray# s -> Int# -> State# s -> State# s
prefetchMutableByteArray1# a1 a2 a3 = GHC.Prim.prefetchMutableByteArray1# a1 a2 a3
{-# NOINLINE prefetchAddr1# #-}
prefetchAddr1# :: Addr# -> Int# -> State# s -> State# s
prefetchAddr1# a1 a2 a3 = GHC.Prim.prefetchAddr1# a1 a2 a3
{-# NOINLINE prefetchValue1# #-}
prefetchValue1# :: a -> State# s -> State# s
prefetchValue1# a1 a2 = GHC.Prim.prefetchValue1# a1 a2
{-# NOINLINE prefetchByteArray0# #-}
prefetchByteArray0# :: ByteArray# -> Int# -> State# s -> State# s
prefetchByteArray0# a1 a2 a3 = GHC.Prim.prefetchByteArray0# a1 a2 a3
{-# NOINLINE prefetchMutableByteArray0# #-}
prefetchMutableByteArray0# :: MutableByteArray# s -> Int# -> State# s -> State# s
prefetchMutableByteArray0# a1 a2 a3 = GHC.Prim.prefetchMutableByteArray0# a1 a2 a3
{-# NOINLINE prefetchAddr0# #-}
prefetchAddr0# :: Addr# -> Int# -> State# s -> State# s
prefetchAddr0# a1 a2 a3 = GHC.Prim.prefetchAddr0# a1 a2 a3
{-# NOINLINE prefetchValue0# #-}
prefetchValue0# :: a -> State# s -> State# s
prefetchValue0# a1 a2 = GHC.Prim.prefetchValue0# a1 a2
