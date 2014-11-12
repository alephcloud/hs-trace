{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trans.Trace
( TraceT(..)
, runTraceT
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Trace.Class
import Control.Monad.Trace.ErrorTrace
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Identity
import Data.Sequence hiding (empty)

newtype TraceT t e m α
  = TraceT
  { _traceT ∷ EitherT (State (Seq t) e) m α
  } deriving (Functor, Monad, Applicative, MonadIO, MonadTrans, MonadBase b)

runTraceT
  ∷ ( Functor m
    , Monad m
    )
  ⇒ TraceT t e m α
  → EitherT (ErrorTrace t e) m α
runTraceT =
  bimapEitherT (uncurry ErrorTrace . flip runState empty) id
  . _traceT

instance Functor m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t =
    TraceT . bimapEitherT (withState (|> t)) id . _traceT

instance MonadTransControl (TraceT t e) where
  newtype StT (TraceT t e) a = StTraceT { unStTraceT ∷ StT (EitherT (State (Seq t) e)) a }
  liftWith = defaultLiftWith TraceT _traceT StTraceT
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT TraceT unStTraceT
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceT t e m) where
  newtype StM (TraceT t e m) a = StMTraceT { unStMTraceT ∷ ComposeSt (TraceT t e) m a }
  liftBaseWith = defaultLiftBaseWith StMTraceT
  {-# INLINE liftBaseWith #-}
  restoreM  = defaultRestoreM unStMTraceT
  {-# INLINE restoreM #-}
