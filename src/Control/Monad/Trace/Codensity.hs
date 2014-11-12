{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trace.Codensity
( putTrace
) where

import Control.Monad.Trace.Class
import Control.Monad.Codensity

-- | Add a trace in synchronous style
putTrace
  ∷ ( Monad m
    , MonadTrace t m
    )
  ⇒ t
  → Codensity m ()
putTrace t =
  Codensity $ \cont →
    traceScope t $
      cont ()
