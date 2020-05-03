{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module MyTransformer where

import           Control.Applicative (liftA2)
import qualified Data.Char           as Char
import           Data.Composition
import           Data.Functor        (($>))

-- Polymorphic in m
class Monad m => MonadReader r m where
  ask :: m r
  ask = reader id
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  reader = (<$> ask)
  {-# MINIMAL (ask | reader), local #-}
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a }
-- We will soon see more instances, but for now let's show
-- that ReaderT is a MonadReader
instance Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local f m = ReaderT $ runReaderT m . f

type Writer w = WriterT w Identity
type State w = StateT w Identity
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m
mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f $ runWriterT m
mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

class (Monoid w, Monad m) => MonadWriter w m where
  writer :: (a, w) -> m a
  writer (a, w) = tell w $> a
  tell :: w -> m ()
  tell = writer . ((), )
  listen :: m a -> m (a, w)
  pass :: m (a, w -> w) -> m a
  {-# MINIMAL (writer | tell), listen, pass #-}
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
mapFirst f (a, b) = (f a, b)
wrapFmapUnwrapWT f = WriterT . fmap f . runWriterT
instance Functor m => Functor (WriterT w m) where
  fmap f = WriterT . fmap (mapFirst f) . runWriterT
swappedPure :: Monoid w => a -> (a, w)
swappedPure = swap . pure where swap (a, b) = (b, a)
instance (Applicative m, Monoid w) => Applicative (WriterT w m) where
  pure = WriterT . pure . swappedPure
  (<*>) = WriterT . runWriterT .: (<*>)
instance (Monad m, Monoid w) => Monad (WriterT w m) where
  (>>=) = WriterT . runWriterT .: (>>=)

instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
  writer = WriterT . return
  listen = mapWriterT (fmap (\(a, w) -> ((a, w), w)))
  pass = mapWriterT (fmap (\((a, f), w) -> (a, f w)))

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
instance Functor m => Functor (StateT w m) where
  fmap f = StateT . (fmap (mapFirst f) .: (runStateT))
instance Applicative m => Applicative (StateT w m) where
  pure = StateT . (pure .: (,))
  (<*>) = StateT . runStateT .: (<*>)
instance Monad m => Monad (StateT w m) where
  (>>=) = StateT . runStateT .: (>>=)

class Monad m => MonadState s m where
  get :: m s
  get = state $ \s -> (s, s)
  put :: s -> m ()
  put s = state $ \_ -> ((), s)
  state :: (s -> (a, s)) -> m a
  state f = do
    oldState <- get
    let (a, newState) = f oldState
    put newState
    return a
  {-# MINIMAL state | (get, put) #-}

instance Monad m => MonadState s (StateT s m) where
  state = StateT . (return .)


newtype Identity a = Identity {runIdentity :: a}
type Reader r = ReaderT r Identity
data DbConfig = DbConfig ()
type DbFetcher a = IO (Reader DbConfig (Maybe a))

-- runMaybeT *unstacks* the monads, returns a nested monad
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}
instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT
instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (<*>) = MaybeT . runMaybeT .: (<*>)
instance Monad m => Monad (MaybeT m) where
  (>>=) = MaybeT . runMaybeT .: (>>=)
type MaybeIO = MaybeT IO
sumTwo :: MaybeIO Int -> MaybeIO Int -> MaybeIO Int
sumTwo = liftA2 (+)

-- Recall that Reader r a = (->) r a

instance Functor m => Functor (ReaderT r m) where
  fmap f = ReaderT . fmap (fmap f) . runReaderT
instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (<*>) = ReaderT . runReaderT .: (<*>)
instance Monad m => Monad (ReaderT r m) where
  (>>=) = ReaderT . runReaderT .: (>>=)

instance MonadReader r m => MonadReader r (StateT s m) where
  ask = lift ask
  local = mapStateT . local
instance (MonadReader r m, Monoid w) => MonadReader r (WriterT w m) where
  ask = lift ask
  local = mapWriterT . local

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  writer = lift . writer
  listen = mapReaderT listen
  pass = mapReaderT pass
instance MonadWriter w m => MonadWriter w (StateT s m) where
  writer = lift . writer
  listen m = StateT $ \s -> do
    ((a, s'), w) <- listen $ runStateT m s
    return ((a, w), s')
  pass m = StateT $ \s -> pass $ do
    ((a, f), s') <- runStateT m s
    return ((a, s'), f)

instance MonadState s m => MonadState s (ReaderT t m) where
  state = lift . state
instance (MonadState s m, Monoid w) => MonadState s (WriterT w m) where
  state = lift . state

--instance (MonadWriter r m) => MonadWriter r (ReaderT w m) where
--  ask = lift ask
--  local = mapWriterT . local

-- We have IO (Reader DbConfig (Maybe a))
type MaybeReaderIO = MaybeT (ReaderT DbConfig IO)


newtype IOMaybe a = IOMaybe { runMaybeIO :: Maybe (IO a)}
--instance Monad IOMaybe where
--  IOMaybe Nothing >>= _ = IOMaybe Nothing -- That was a freebie
--  IOMaybe (Just x) >>= f = let y = fmap f x in undefined
--
--liftMaybe :: Monad m => m a -> MaybeT m a
--liftMaybe = MaybeT . fmap Just
--type DbReader = Reader DbConfig
--read5 :: DbReader Int
--read5 = return 5
---- As expected, nested monads need manual nesting fmaps etc.
--readJust5 :: DbReader (Maybe Int)
--readJust5 = fmap Just read5
--readMaybeT5 :: MaybeT DbReader Int
--readMaybeT5 = liftMaybe read5
--
class MonadTrans t where
  lift :: Monad m => m a -> t m a
instance MonadTrans MaybeT where
  lift = MaybeT . fmap return
instance MonadTrans (ReaderT r) where
  lift = ReaderT . return

instance Monoid w => MonadTrans (WriterT w) where
  lift = WriterT . fmap swappedPure
instance MonadTrans (StateT w) where
  lift m = StateT $ \s -> fmap (, s) m
--
--askT :: Monad m => ReaderT r m r
--askT = ReaderT return
--sumEnvironmentAndInput :: ReaderT Int IO Int
--sumEnvironmentAndInput = do
--  x <- ask :: (ReaderT Int IO Int)
--  y <- lift $ Char.digitToInt <$> getChar :: (ReaderT Int IO Int)
--  return $ x + y

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where  
  liftIO = id
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
