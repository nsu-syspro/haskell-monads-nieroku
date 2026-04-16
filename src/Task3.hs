{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Control.Monad (join)
import Data.Functor.Identity

-- * Functor composition

-- | Represents composition of two functors.
newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f = Compose . (fmap (fmap f)) . getCompose

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . (pure . pure)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose (liftA2 (liftA2 id) fgab fga)

-- * Monad composition

instance (Monad m, Monad n, Distrib n m) => Monad (Compose m n) where
  (>>=) :: Compose m n a -> (a -> Compose m n b) -> Compose m n b
  m >>= f = join' (fmap f m)
    where
      join' :: Compose m n (Compose m n a) -> Compose m n a
      join' = Compose . fmap join . join . fmap distrib . getCompose . fmap getCompose

-- * Distributive property

-- | Describes distributive property of two monads.
class (Monad m, Monad n) => Distrib m n where
  distrib :: m (n a) -> n (m a)

-- * Distributive instances

instance (Monad n) => Distrib Identity n where
  distrib :: (Monad n) => Identity (n a) -> n (Identity a)
  distrib = sequenceA

instance (Monad n) => Distrib Maybe n where
  distrib :: Maybe (n a) -> n (Maybe a)
  distrib = sequenceA

instance (Monad n) => Distrib [] n where
  distrib :: [] (n a) -> n ([] a)
  distrib = sequenceA

instance (Monad n, Monoid e) => Distrib ((,) e) n where
  distrib :: (e, n a) -> n (e, a)
  distrib (e, n) = fmap ((,) e) n

instance (Monad n) => Distrib n ((->) e) where
  distrib :: n (e -> a) -> (e -> n a)
  distrib n e = fmap ($ e) n
