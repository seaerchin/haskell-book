{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Chap26 where

-- m wraps around the either monad
-- a is not part of the structure
newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f e@(EitherT ema) = do
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
    f mea = do
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
