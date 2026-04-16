{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Hide built-in bind definition

import Data.Functor.Identity
import Data.Maybe
import Prelude hiding ((>>=))

-- * Kleisli composition monad

-- | Monad based on Kleisli composition '(>=>)' operator
-- instead of usual bind operator '(>>=)'.
class (Applicative m) => KleisliMonad m where
  infixr 1 >=>
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- * Equivalent views

infixl 1 >>=

(>>=) :: (KleisliMonad m) => m a -> (a -> m b) -> m b
(>>=) = flip (id >=>)

join :: (KleisliMonad m) => m (m a) -> m a
join = id >=> id

-- * Instances

instance KleisliMonad Identity where
  (>=>) :: (a -> Identity b) -> (b -> Identity c) -> (a -> Identity c)
  f >=> g = runIdentity . (fmap g) . f

instance KleisliMonad Maybe where
  (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
  f >=> g = fromMaybe Nothing . (fmap g) . f

instance KleisliMonad [] where
  (>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
  f >=> g = concat . (fmap g) . f

instance (Monoid e) => KleisliMonad ((,) e) where
  (>=>) :: (Monoid e) => (a -> (e, b)) -> (b -> (e, c)) -> (a -> (e, c))
  f >=> g = (\(m1, (m2, c)) -> (m1 <> m2, c)) . (fmap g) . f

instance KleisliMonad ((->) e) where
  (>=>) :: (a -> e -> b) -> (b -> e -> c) -> (a -> e -> c)
  f >=> g = (\h e -> h e e) . (fmap g) . f
