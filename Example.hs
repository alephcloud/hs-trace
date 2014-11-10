{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module Example where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Either
import Control.Monad.State
import Control.Lens hiding (Snoc)

data Void

data Tags
  = FrontEnd
  | BackEnd
  | SetPassword
  | GetUserInfo
  | Dynamo
  deriving Show

newtype TraceT t e m α
  = TraceT
  { _trace ∷ StateT [t] (EitherT e m) α
  } deriving (Functor, Monad, Applicative, MonadState [t], MonadTrace t, MonadError e, MonadIO)

class MonadTrace t m | m → t where
  traceScope
    ∷ t
    → m α
    → m α

instance MonadTrace t (StateT [t] m) where
  traceScope t = withStateT (t:)

test = do
  traceScope FrontEnd $
    traceScope GetUserInfo $ do
      liftIO $ print ()
      throwError ()
