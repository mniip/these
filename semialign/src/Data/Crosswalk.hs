{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Crosswalk (
    -- * Crosswalk
    Crosswalk (..),
    -- * Bicrosswalk
    Bicrosswalk (..),
    ) where

import Control.Applicative   (pure, (<$>), Const(..))
import Data.Bifoldable       (Bifoldable (..))
import Data.Bifunctor        (Bifunctor (..))
import Data.Foldable         (Foldable (..))
import Data.Functor.Compose  (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Sum      (Sum (..))
import Data.Functor.These    (These1 (..))
import Data.Proxy            (Proxy (..))
import Data.Vector.Generic   (Vector)
import Prelude               (Either (..), Functor (fmap), Maybe (..), id, (.))

import qualified Data.List.NonEmpty  as NE
import qualified Data.Sequence       as Seq
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG

import Data.Align
import Data.These

-- --------------------------------------------------------------------------
-- | Foldable functors supporting traversal through an alignable
--   functor.
--
--   Minimal definition: @crosswalk@ or @sequenceL@.
--
--   Laws:
--
-- @
-- crosswalk (const nil) = const nil
-- crosswalk f = sequenceL . fmap f
-- @
class (Functor t, Foldable t) => Crosswalk t where
    crosswalk :: (Align f) => (a -> f b) -> t a -> f (t b)
    crosswalk f = sequenceL . fmap f

    sequenceL :: (Align f) => t (f a) -> f (t a)
    sequenceL = crosswalk id

    {-# MINIMAL crosswalk | sequenceL #-}

instance Crosswalk Identity where
    crosswalk f (Identity a) = fmap Identity (f a)

instance Crosswalk Maybe where
    crosswalk _ Nothing = nil
    crosswalk f (Just a) = Just <$> f a

instance Crosswalk [] where
    crosswalk _ [] = nil
    crosswalk f (x:xs) = alignWith cons (f x) (crosswalk f xs)
      where cons = these pure id (:)

instance Crosswalk NE.NonEmpty where
    crosswalk f (x NE.:| []) = (NE.:| []) <$> f x
    crosswalk f (x1 NE.:| x2 : xs) = alignWith cons (f x1) (crosswalk f (x2 NE.:| xs))
      where cons = these (NE.:| []) id (NE.<|)

instance Crosswalk Seq.Seq where
    crosswalk f = foldr (alignWith cons . f) nil where
        cons = these Seq.singleton id (Seq.<|)

crosswalkVector :: (Vector v a, Vector v b, Align f)
    => (a -> f b) -> v a -> f (v b)
crosswalkVector f = fmap VG.fromList . VG.foldr (alignWith cons . f) nil where
    cons = these pure id (:)

instance Crosswalk V.Vector where
    crosswalk = crosswalkVector

instance Crosswalk (Either e) where
    crosswalk _ (Left _) = nil
    crosswalk f (Right x) = Right <$> f x

instance Crosswalk (These a) where
    crosswalk _ (This _) = nil
    crosswalk f (That x) = That <$> f x
    crosswalk f (These a x) = These a <$> f x

instance Crosswalk ((,) a) where
    crosswalk fun (a, x) = fmap ((,) a) (fun x)

-- can't (shouldn't) do longer tuples until there are Functor and Foldable
-- instances for them

instance Crosswalk Proxy where
    crosswalk _ _ = nil

instance Crosswalk (Const r) where
    crosswalk _ _ = nil

instance (Crosswalk f, Crosswalk g) => Crosswalk (Sum f g) where
    crosswalk f (InL xs) = InL <$> crosswalk f xs
    crosswalk f (InR xs) = InR <$> crosswalk f xs

instance (Crosswalk f, Crosswalk g) => Crosswalk (These1 f g) where
    crosswalk f (This1 xs) = This1 <$> crosswalk f xs
    crosswalk f (That1 ys) = That1 <$> crosswalk f ys
    crosswalk f (These1 xs ys) = alignWith go (crosswalk f xs) (crosswalk f ys)
      where go = these This1 That1 These1

instance (Crosswalk f, Crosswalk g) => Crosswalk (Compose f g) where
    crosswalk f
        = fmap Compose -- can't coerce: maybe the Align-able thing has role nominal
        . crosswalk (crosswalk f)
        . getCompose

-- --------------------------------------------------------------------------
-- | Bifoldable bifunctors supporting traversal through an alignable
--   functor.
--
--   Minimal definition: @bicrosswalk@ or @bisequenceL@.
--
--   Laws:
--
-- @
-- bicrosswalk (const empty) (const empty) = const empty
-- bicrosswalk f g = bisequenceL . bimap f g
-- @
class (Bifunctor t, Bifoldable t) => Bicrosswalk t where
    bicrosswalk :: (Align f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
    bicrosswalk f g = bisequenceL . bimap f g

    bisequenceL :: (Align f) => t (f a) (f b) -> f (t a b)
    bisequenceL = bicrosswalk id id

    {-# MINIMAL bicrosswalk | bisequenceL #-}

instance Bicrosswalk Either where
    bicrosswalk f _ (Left x)  = Left  <$> f x
    bicrosswalk _ g (Right x) = Right <$> g x

instance Bicrosswalk These where
    bicrosswalk f _ (This x) = This <$> f x
    bicrosswalk _ g (That x) = That <$> g x
    bicrosswalk f g (These x y) = align (f x) (g y)

instance Bicrosswalk Const where
    bicrosswalk f _ (Const x) = Const <$> f x
