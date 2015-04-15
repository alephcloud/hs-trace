{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trace.Class
( MonadTrace(..)
) where

import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe

import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy

import Data.Sequence
import Data.Monoid

-- | A class for monads that have a scoped tracing effect
class Monad m ⇒ MonadTrace t m | m → t where
  -- | Add a tag or breadcrumb to a scope
  traceScope
    ∷ t
    → m α
    → m α

  -- | Read back your own trace
  readTrace
    ∷ m (Seq t)

instance MonadTrace t m ⇒ MonadTrace t (ReaderT r m) where
  traceScope t (ReaderT m) = ReaderT $ traceScope t . m
  readTrace = lift readTrace

instance (Monoid w, MonadTrace t m) ⇒ MonadTrace t (Strict.WriterT w m) where
  traceScope t = Strict.WriterT . traceScope t . Strict.runWriterT
  readTrace = lift readTrace

instance (Monoid w, MonadTrace t m) ⇒ MonadTrace t (Lazy.WriterT w m) where
  traceScope t = Lazy.WriterT . traceScope t . Lazy.runWriterT
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (Strict.StateT w m) where
  traceScope t (Strict.StateT m) = Strict.StateT $ traceScope t . m
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (Lazy.StateT w m) where
  traceScope t (Lazy.StateT m) = Lazy.StateT $ traceScope t . m
  readTrace = lift readTrace

instance (Monoid w, MonadTrace t m) ⇒ MonadTrace t (Strict.RWST r w s m) where
  traceScope t (Strict.RWST m) = Strict.RWST $ \r → traceScope t . m r
  readTrace = lift readTrace

instance (Monoid w, MonadTrace t m) ⇒ MonadTrace t (Lazy.RWST r w s m) where
  traceScope t (Lazy.RWST m) = Lazy.RWST $ \r → traceScope t . m r
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (ExceptT e m) where
  traceScope t = ExceptT . traceScope t . runExceptT
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (IdentityT m) where
  traceScope t = IdentityT . traceScope t . runIdentityT
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (ContT r m) where
  traceScope t (ContT m) = ContT $ traceScope t . m
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (ListT m) where
  traceScope t = ListT . traceScope t . runListT
  readTrace = lift readTrace

instance MonadTrace t m ⇒ MonadTrace t (MaybeT m) where
  traceScope t = MaybeT . traceScope t . runMaybeT
  readTrace = lift readTrace

