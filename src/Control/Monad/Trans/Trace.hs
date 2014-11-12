{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trans.Trace
( TraceT
, runTraceT
, readTrace
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Trace.Class
import Control.Monad.Trace.ErrorTrace
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Sequence as S

-- | A concrete monad transformer @'TraceT' t e m@ where @t@ is the type of
-- tags/breadcrumbs, @e@ is the type of errors, and @m@ is the underlying monad.
--
newtype TraceT t e m α
  = TraceT
  { _traceT ∷ EitherT e (StateT (Seq t) m) α
  } deriving (Functor, Monad, Applicative, Alternative, MonadIO, MonadBase b, MonadError e)

instance MonadTrans (TraceT t e) where
  lift = TraceT . EitherT . (>>= return . Right) . lift

instance Monad m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t =
    TraceT . mapEitherT (withStateT (|> t)) . _traceT
  readTrace =
    TraceT . EitherT $
      get >>= return . Right

-- | Run a traced traced computation to get either its result, or an error and
-- its provenience ('ErrorTrace').
--
runTraceT
  ∷ ( Functor m
    , Monad m
    )
  ⇒ TraceT t e m α
  → EitherT (ErrorTrace t e) m α
runTraceT (TraceT m) = do
  (result, trace) ← lift $ runStateT (runEitherT m) S.empty
  either (left . flip ErrorTrace trace) right result

instance MonadTransControl (TraceT t e) where
  newtype StT (TraceT t e) α = StTraceT { unStTraceT ∷ StT (StateT (Seq t)) (StT (EitherT e) α) }
  liftWith f = TraceT . liftWith $ \run → liftWith $ \run' → f $ liftM StTraceT . run' . run . _traceT
  {-# INLINE liftWith #-}
  restoreT = TraceT . restoreT . restoreT . liftM unStTraceT
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceT t e m) where
  newtype StM (TraceT t e m) α = StMTraceT { unStMTraceT ∷ ComposeSt (TraceT t e) m α }
  liftBaseWith = defaultLiftBaseWith StMTraceT
  {-# INLINE liftBaseWith #-}
  restoreM  = defaultRestoreM unStMTraceT
  {-# INLINE restoreM #-}
