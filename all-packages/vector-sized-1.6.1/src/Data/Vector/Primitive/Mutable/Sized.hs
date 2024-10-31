{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

{-|
This module re-exports the functionality in 'Data.Vector.Generic.Mutable.Sized'
 specialized to 'Data.Vector.Primitive.Mutable'.

Functions returning a vector determine the size from the type context unless
they have a @'@ suffix in which case they take an explicit 'Proxy' argument.

Functions where the resulting vector size is not known until runtime are
not exported.
-}

module Data.Vector.Primitive.Mutable.Sized
 ( MVector
   -- * Accessors
   -- ** Length information
  , length
  , length'
  , null
   -- ** Extracting subvectors
  , slice
  , slice'
  , init
  , tail
  , take
  , take'
  , drop
  , drop'
  , splitAt
  , splitAt'
  -- ** Overlaps
  , overlaps
  -- * Construction
  -- ** Initialisation
  , new
  , unsafeNew
  , replicate
  , replicate'
  , replicateM
  , replicateM'
  , clone
  -- ** Growing
  , grow
  , growFront
  -- ** Restricting memory usage
  , clear
  -- * Accessing individual elements
  , read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , exchange
  , exchange'
  , unsafeRead
  , unsafeWrite
  , unsafeModify
  , unsafeSwap
  , unsafeExchange
  -- * Modifying vectors
  , nextPermutation
  -- ** Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
    -- * Conversions
    -- ** Unsized Mutable Vectors
  , toSized
  , withSized
  , fromSized
  ) where

import qualified Data.Vector.Generic.Mutable.Sized as VGM
import qualified Data.Vector.Primitive.Mutable as VSM
import GHC.TypeLits
import Data.Finite
import Data.Primitive (Prim)
import Data.Proxy
import Control.Monad.Primitive
import Prelude hiding ( length, null, replicate, init,
                        tail, take, drop, splitAt, read )


-- | 'Data.Vector.Generic.Mutable.Sized.Vector' specialized to use
-- 'Data.Vector.Primitive.Mutable'.
type MVector = VGM.MVector VSM.MVector

-- * Accessors

-- ** Length information

-- | /O(1)/ Yield the length of the mutable vector as an 'Int'.
length :: forall n s a. (KnownNat n)
       => MVector n s a -> Int
length = VGM.length
{-# inline length #-}

-- | /O(1)/ Yield the length of the mutable vector as a 'Proxy'.
length' :: forall n s a. ()
        => MVector n s a -> Proxy n
length' = VGM.length'
{-# inline length' #-}

-- | /O(1)/ Check whether the mutable vector is empty.
null :: forall n s a. (KnownNat n)
       => MVector n s a -> Bool
null = VGM.null
{-# inline null #-}

-- ** Extracting subvectors

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- inferred length argument.
slice :: forall i n k s a p. (KnownNat i, KnownNat n, Prim a)
      => p i -- ^ starting index
      -> MVector (i+n+k) s a
      -> MVector n s a
slice = VGM.slice
{-# inline slice #-}

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- explicit length argument.
slice' :: forall i n k s a p
        . (KnownNat i, KnownNat n, Prim a)
       => p i -- ^ starting index
       -> p n -- ^ length
       -> MVector (i+n+k) s a
       -> MVector n s a
slice' = VGM.slice'
{-# inline slice' #-}

-- | /O(1)/ Yield all but the last element of a non-empty mutable vector
-- without copying.
init :: forall n s a. Prim a
     => MVector (n+1) s a -> MVector n s a
init = VGM.init
{-# inline init #-}

-- | /O(1)/ Yield all but the first element of a non-empty mutable vector
-- without copying.
tail :: forall n s a. Prim a
     => MVector (1+n) s a -> MVector n s a
tail = VGM.tail
{-# inline tail #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is inferred from the
-- type.
take :: forall n k s a. (KnownNat n, Prim a)
     => MVector (n+k) s a -> MVector n s a
take = VGM.take
{-# inline take #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is given explicitly
-- as a 'Proxy' argument.
take' :: forall n k s a p. (KnownNat n, Prim a)
      => p n -> MVector (n+k) s a -> MVector n s a
take' = VGM.take'
{-# inline take' #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- inferred from the type.
drop :: forall n k s a. (KnownNat n, Prim a)
     => MVector (n+k) s a -> MVector k s a
drop = VGM.drop
{-# inline drop #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- givel explicitly as a 'Proxy' argument.
drop' :: forall n k s a p. (KnownNat n, Prim a)
      => p n -> MVector (n+k) s a -> MVector k s a
drop' = VGM.drop'
{-# inline drop' #-}

-- | /O(1)/ Yield the first @n@ elements, paired with the rest, without copying.
-- The lengths of the resulting vectors are inferred from the type.
splitAt :: forall n m s a. (KnownNat n, Prim a)
        => MVector (n+m) s a -> (MVector n s a, MVector m s a)
splitAt = VGM.splitAt
{-# inline splitAt #-}

-- | /O(1)/ Yield the first @n@ elements, paired with the rest, without
-- copying.  The length of the first resulting vector is passed explicitly as a
-- 'Proxy' argument.
splitAt' :: forall n m s a p. (KnownNat n, Prim a)
         => p n -> MVector (n+m) s a -> (MVector n s a, MVector m s a)
splitAt' = VGM.splitAt'
{-# inline splitAt' #-}

-- ** Overlaps

-- | /O(1)/ Check if two vectors overlap.
overlaps :: forall n k s a. Prim a
         => MVector n s a
         -> MVector k s a
         -> Bool
overlaps = VGM.overlaps
{-# inline overlaps #-}

-- * Construction

-- ** Initialisation

-- | Create a mutable vector where the length is inferred from the type.
new :: forall n m a. (KnownNat n, PrimMonad m, Prim a)
    => m (MVector n (PrimState m) a)
new = VGM.new
{-# inline new #-}

-- | Create a mutable vector where the length is inferred from the type.
-- The memory is not initialized.
unsafeNew :: forall n m a. (KnownNat n, PrimMonad m, Prim a)
          => m (MVector n (PrimState m) a)
unsafeNew = VGM.unsafeNew
{-# inline unsafeNew #-}

-- | Create a mutable vector where the length is inferred from the type and
-- fill it with an initial value.
replicate :: forall n m a. (KnownNat n, PrimMonad m, Prim a)
          => a -> m (MVector n (PrimState m) a)
replicate = VGM.replicate
{-# inline replicate #-}

-- | Create a mutable vector where the length is given explicitly as
-- a 'Proxy' argument and fill it with an initial value.
replicate' :: forall n m a p. (KnownNat n, PrimMonad m, Prim a)
           => p n -> a -> m (MVector n (PrimState m) a)
replicate' = VGM.replicate'
{-# inline replicate' #-}

-- | Create a mutable vector where the length is inferred from the type and
-- fill it with values produced by repeatedly executing the monadic action.
replicateM :: forall n m a. (KnownNat n, PrimMonad m, Prim a)
           => m a -> m (MVector n (PrimState m) a)
replicateM = VGM.replicateM
{-# inline replicateM #-}

-- | Create a mutable vector where the length is given explicitly as
-- a 'Proxy' argument and fill it with values produced by repeatedly
-- executing the monadic action.
replicateM' :: forall n m a p. (KnownNat n, PrimMonad m, Prim a)
           => p n -> m a -> m (MVector n (PrimState m) a)
replicateM' = VGM.replicateM'
{-# inline replicateM' #-}

-- | Create a copy of a mutable vector.
clone :: forall n m a. (PrimMonad m, Prim a)
      => MVector n (PrimState m) a -> m (MVector n (PrimState m) a)
clone = VGM.clone
{-# inline clone #-}

-- ** Growing

-- | Grow a mutable vector by an amount given explicitly as a 'Proxy'
-- argument.
grow :: forall n k m a p. (KnownNat k, PrimMonad m, Prim a)
      => p k -> MVector n (PrimState m) a -> m (MVector (n + k) (PrimState m) a)
grow = VGM.grow
{-# inline grow #-}

-- | Grow a mutable vector (from the front) by an amount given explicitly
-- as a 'Proxy' argument.
growFront :: forall n k m a p. (KnownNat k, PrimMonad m, Prim a)
      => p k -> MVector n (PrimState m) a -> m (MVector (n + k) (PrimState m) a)
growFront = VGM.growFront
{-# inline growFront #-}

-- ** Restricting memory usage

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects.
clear :: (PrimMonad m, Prim a) => MVector n (PrimState m) a -> m ()
clear = VGM.clear
{-# inline clear #-}

-- * Accessing individual elements

-- | /O(1)/ Yield the element at a given type-safe position using 'Finite'.
read :: forall n m a. (PrimMonad m, Prim a)
      => MVector n (PrimState m) a -> Finite n -> m a
read = VGM.read
{-# inline read #-}

-- | /O(1)/ Yield the element at a given type-safe position using 'Proxy'.
read' :: forall n k a m p. (KnownNat k, PrimMonad m, Prim a)
       => MVector (n+k+1) (PrimState m) a -> p k -> m a
read' = VGM.read'
{-# inline read' #-}

-- | /O(1)/ Yield the element at a given 'Int' position without bounds
-- checking.
unsafeRead :: forall n a m. (PrimMonad m, Prim a)
           => MVector n (PrimState m) a -> Int -> m a
unsafeRead = VGM.unsafeRead
{-# inline unsafeRead #-}

-- | /O(1)/ Replace the element at a given type-safe position using 'Finite'.
write :: forall n m a. (PrimMonad m, Prim a)
      => MVector n (PrimState m) a -> Finite n -> a -> m ()
write = VGM.write
{-# inline write #-}

-- | /O(1)/ Replace the element at a given type-safe position using 'Proxy'.
write' :: forall n k a m p. (KnownNat k, PrimMonad m, Prim a)
       => MVector (n+k+1) (PrimState m) a -> p k -> a -> m ()
write' = VGM.write'
{-# inline write' #-}

-- | /O(1)/ Replace the element at a given 'Int' position without bounds
-- checking.
unsafeWrite :: forall n m a. (PrimMonad m, Prim a)
      => MVector n (PrimState m) a -> Int -> a -> m ()
unsafeWrite = VGM.unsafeWrite
{-# inline unsafeWrite #-}

-- | /O(1)/ Modify the element at a given type-safe position using 'Finite'.
modify :: forall n m a. (PrimMonad m, Prim a)
       => MVector n (PrimState m) a -> (a -> a) -> Finite n -> m ()
modify = VGM.modify
{-# inline modify #-}

-- | /O(1)/ Modify the element at a given type-safe position using 'Proxy'.
modify' :: forall n k a m p. (KnownNat k, PrimMonad m, Prim a)
        => MVector (n+k+1) (PrimState m) a -> (a -> a) -> p k -> m ()
modify' = VGM.modify'
{-# inline modify' #-}

-- | /O(1)/ Modify the element at a given 'Int' position without bounds
-- checking.
unsafeModify :: forall n m a. (PrimMonad m, Prim a)
       => MVector n (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify = VGM.unsafeModify
{-# inline unsafeModify #-}

-- | /O(1)/ Swap the elements at the given type-safe positions using 'Finite's.
swap :: forall n m a. (PrimMonad m, Prim a)
     => MVector n (PrimState m) a -> Finite n -> Finite n -> m ()
swap = VGM.swap
{-# inline swap #-}

-- | /O(1)/ Swap the elements at the given 'Int' positions without bounds
-- checking.
unsafeSwap :: forall n m a. (PrimMonad m, Prim a)
           => MVector n (PrimState m) a -> Int -> Int -> m ()
unsafeSwap = VGM.unsafeSwap
{-# inline unsafeSwap #-}

-- | /O(1)/ Replace the element at a given type-safe position and return
-- the old element, using 'Finite'.
exchange :: forall n m a. (PrimMonad m, Prim a)
         => MVector n (PrimState m) a -> Finite n -> a -> m a
exchange = VGM.exchange
{-# inline exchange #-}

-- | /O(1)/ Replace the element at a given type-safe position and return
-- the old element, using 'Finite'.
exchange' :: forall n k a m p. (KnownNat k, PrimMonad m, Prim a)
          => MVector (n+k+1) (PrimState m) a -> p k -> a -> m a
exchange' = VGM.exchange'
{-# inline exchange' #-}

-- | /O(1)/ Replace the element at a given 'Int' position and return
-- the old element. No bounds checks are performed.
unsafeExchange :: forall n m a. (PrimMonad m, Prim a)
         => MVector n (PrimState m) a -> Int -> a -> m a
unsafeExchange = VGM.unsafeExchange
{-# inline unsafeExchange #-}

-- * Modifying vectors

-- | Compute the next permutation (lexicographically) of a given vector
-- in-place.  Returns 'False' when the input is the last permutation.
nextPermutation :: forall n e m. (Ord e, PrimMonad m, Prim e)
                => MVector n (PrimState m) e -> m Bool
nextPermutation = VGM.nextPermutation
{-# inline nextPermutation #-}

-- ** Filling and copying

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Prim a) => MVector n (PrimState m) a -> a -> m ()
set = VGM.set
{-# inline set #-}

-- | Copy a vector. The two vectors may not overlap.
copy :: (PrimMonad m, Prim a)
     => MVector n (PrimState m) a       -- ^ target
     -> MVector n (PrimState m) a       -- ^ source
     -> m ()
copy = VGM.copy
{-# inline copy #-}

-- * Conversions

-- ** Unsized Mutable Vectors

-- | Copy a vector. The two vectors may not overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Prim a)
           => MVector n (PrimState m) a       -- ^ target
           -> MVector n (PrimState m) a       -- ^ source
           -> m ()
unsafeCopy = VGM.unsafeCopy
{-# inline unsafeCopy #-}

-- | Move the contents of a vector.  If the two vectors do not overlap,
-- this is equivalent to 'copy'.  Otherwise, the copying is performed as if
-- the source vector were copied to a temporary vector and then the
-- temporary vector was copied to the target vector.
move :: (PrimMonad m, Prim a)
     => MVector n (PrimState m) a       -- ^ target
     -> MVector n (PrimState m) a       -- ^ source
     -> m ()
move = VGM.move
{-# inline move #-}

-- | Convert a 'Data.Vector.Primitive.Mutable.MVector' into
-- a 'Data.Vector.Primitive.Mutable.Sized.MVector' if it has the correct
-- size, otherwise return Nothing.
--
-- Note that this does no copying; the returned 'MVector' is a reference to
-- the exact same vector in memory as the given one, and any modifications
-- to it are also reflected in the given
-- 'Data.Vector.Primitive.Mutable.MVector'.
toSized :: forall n a s. (KnownNat n, Prim a)
        => VSM.MVector s a -> Maybe (MVector n s a)
toSized = VGM.toSized
{-# inline toSized #-}

-- | Takes a 'Data.Vector.Primitive.Mutable.MVector' and returns
-- a continuation providing a 'Data.Vector.Primitive.Mutable.Sized.MVector'
-- with a size parameter @n@ that is determined at runtime based on the
-- length of the input vector.
--
-- Essentially converts a 'Data.Vector.Primitive.Mutable.MVector' into
-- a 'Data.Vector.Primitive.Sized.MVector' with the correct size parameter
-- @n@.
--
-- Note that this does no copying; the returned 'MVector' is a reference to
-- the exact same vector in memory as the given one, and any modifications
-- to it are also reflected in the given
-- 'Data.Vector.Primitive.Mutable.MVector'.
withSized :: forall s a r. Prim a
          => VSM.MVector s a -> (forall n. KnownNat n => MVector n s a -> r) -> r
withSized = VGM.withSized
{-# inline withSized #-}

-- | Convert a 'Data.Vector.Primitive.Mutable.Sized.MVector' into a
-- 'Data.Vector.Primitive.Mutable.MVector'.
--
-- Note that this does no copying; the returned
-- 'Data.Vector.Primitive.Mutable.MVector' is a reference to the exact same
-- vector in memory as the given one, and any modifications to it are also
-- reflected in the given 'MVector'.
fromSized :: MVector n s a -> VSM.MVector s a
fromSized = VGM.fromSized
{-# inline fromSized #-}
