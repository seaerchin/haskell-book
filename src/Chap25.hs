{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE InstanceSigs #-}

module Chap25 where

import Data.Char
import Data.Word

newtype Compose f g a = Compose {getCompose :: f (g a)}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  af <*> c@(Compose fga) =
    -- strip 1 layer of structure away
    -- then <*>
    let appF = getCompose af
     in (.) id <$> af <*> c

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f c@(Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f c@(Compose fga) =
    let outer = (traverse . traverse) f fga
        go = \func wrapped -> traverse (traverse func) wrapped
        z = go f fga
        test = Compose <$> outer
     in test