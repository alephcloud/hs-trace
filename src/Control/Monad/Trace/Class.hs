{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Monad.Trace.Class
( MonadTrace(..)
) where

-- | A class for monads that have a scoped tracing effect
class MonadTrace t m | m → t where
  -- | Add a tag or breadcrumb to a scope
  traceScope
    ∷ t
    → m α
    → m α
