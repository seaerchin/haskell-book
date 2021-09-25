{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Chap26 where

-- m wraps around the either monad
-- a is not part of the structure

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.Functor.Identity

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f e@(EitherT ema) =
    f <$> e

instance Applicative m => Applicative (EitherT e m) where
  -- put in base either context, then in wrapper monad context
  -- lastly, wrap up in eitherT context
  pure = EitherT . pure . Right
  f <*> y =
    let base = runEitherT y
        baseF = runEitherT f
        -- equivalent to using the helper function defined below
        t = (<*>) <$> baseF <*> base
     in EitherT t

helper :: Either a (b -> c) -> Either a b -> Either a c
helper f b = f <*> b

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f =
    EitherT $ ema >>= either (return . Left) (runEitherT . f)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . f . runEitherT
  where
    f mea =
      swapEither <$> mea

swapEither :: Either e a -> Either a e
swapEither (Right a) = Left a
swapEither (Left e) = Right e

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT l r = cata . runEitherT
  where
    cata m = do
      base <- m
      either l r base

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f r =
    let base = runReaderT r
     in -- need m a -> m b
        ReaderT $ fmap (fmap f) base

instance (Applicative app) => Applicative (ReaderT r app) where
  pure a = ReaderT (\r -> pure a)
  appF <*> app =
    ReaderT
      ( \r ->
          let baseF = runReaderT appF r
              base = runReaderT app r
           in baseF <*> base
      )

-- ReaderT r m b :: ReaderT (\r -> m b)
instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  ma >>= amf =
    let baseMa = runReaderT ma
        baseF = runReaderT <$> amf
        test = (baseF <$>) <$> baseMa
     in ReaderT (\r -> test r >>= \x -> x r)

-- StateT
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  -- i derived this from the base one i wrote but idk what this actually means lmao
  fmap f m = StateT $ (first f <$>) <$> runStateT m

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  appF <*> app =
    StateT
      ( \s -> do
          f <- runStateT appF s
          b <- runStateT app s
          return $ first (fst f) b
      )

-- wrapping values
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded =
  MaybeT $
    ExceptT
      ( ReaderT
          ( const $
              return $
                Right
                  (Just 1)
          )
      )

-- lifting using monadTrans (lifting 1 layer at a time)
instance MonadTrans (EitherT e) where
  lift m = do
    EitherT $ Right <$> m

instance MonadTrans (StateT s) where
  -- we must inject the monad into the greater context of the transformer
  lift m = StateT (\s -> (\a -> (a, s)) <$> m)
