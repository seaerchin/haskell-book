{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Chap26 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity (IdentityT)
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Bifunctor (first)
import Data.Functor.Identity
import Debug.Trace (trace)
import System.Random.Stateful

-- m wraps around the either monad
-- a is not part of the structure
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

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ return . f

get :: Monad m => StateT s m s
get = state (\s -> (s, s))

put :: Monad m => s -> StateT s m ()
put s = state (const ((), s))

instance (Functor m) => Functor (StateT s m) where
  -- i derived this from the base one i wrote but idk what this actually means lmao
  fmap f m = StateT $ (first f <$>) <$> runStateT m

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  appF <*> app =
    StateT
      ( \s -> do
          f <- runStateT appF s
          as <- runStateT app (snd f)
          let b = fst f $ fst as
          return (b, snd as)
      )

instance (Monad m) => Monad (StateT s m) where
  return = pure
  tma >>= ftma =
    StateT
      ( \x -> do
          initial <- runStateT tma x
          runStateT (ftma . fst $ initial) $ snd initial
      )

-- redefining the transformer here to allow us to write our own instances for monadIO
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
  fmap f m =
    let b = runMaybeT m
     in MaybeT $ (fmap . fmap) f b

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  af <*> a =
    let mf = runMaybeT af
        ma = runMaybeT a
     in -- we need to remove 1 layer of structure (monad m)
        -- this is done using fmap
        -- next, functions embedded in the structure (Maybe) need to communicate values in the structure (Maybe)
        -- this is done using <*>
        MaybeT $ fmap (<*>) mf <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  maybeT >>= mf =
    let m = runMaybeT maybeT
        -- type i want is s :: m (Maybe b)
        s = (fmap . fmap) mf m
     in MaybeT $
          -- unwrap s
          -- pattern match on s and act accordingly
          s >>= \case
            Nothing -> return Nothing
            Just mt -> runMaybeT mt

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

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (const m)

-- lifting using monadIO; given that we have some structure with IO capabilities, lift an IO action into that structure.
instance (MonadIO m) => MonadIO (EitherT e m) where
  -- inject an io action into the structure encapsulated by the transformer
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

-- utilize monad m's liftIO method to lift the action into our external monad
-- the lambad is equivalent to lift because we are just lifting the external monad into MaybeT
-- (\x -> MaybeT $ Just <$> x) . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

rDec :: Num a => ReaderT a Identity a
rDec = ReaderT $ return . (+ (-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT
    ( \x -> do
        print ("HI: " ++ show x)
        return $ x + 1
    )

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  base <- get
  let x = show base
  liftIO $ print ("HI: " ++ x)
  put (base + 2)
  return x

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  Chap26.guard $ isValid v
  return v

guard :: (Monad m) => Bool -> MaybeT m Bool
guard pred = if pred then lift $ return pred else MaybeT $ return Nothing

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

-- also evaluated twice??
guardInput :: MaybeT IO Int
guardInput = do
  input <- liftIO getLine
  liftIO $ print "within maybeT"
  let int = read input :: Int
  Chap26.guard ((int < 6) && (int > 0))
  return int

test :: IO ()
test = do
  a <- runMaybeT $ runStateT morra []
  putStrLn "wutface"
  return ()

morra :: StateT [(Int, Int)] (MaybeT IO) ()
morra = forever $ do
  liftIO $ putStrLn "Input your options"
  input <- lift guardInput
  compInt <- liftIO $ randomRIO (0, 5)
  appendState (input, compInt)
  cur <- get
  liftIO $ putStrLn ("Player: " ++ show input)
  liftIO $ putStrLn ("Computer: " ++ show compInt)
  liftIO $ print cur
  return ()

appendState :: (MonadIO m) => s -> StateT [s] m ()
appendState s = do
  base <- get
  liftIO $ print "here"
  put (base ++ [s])
