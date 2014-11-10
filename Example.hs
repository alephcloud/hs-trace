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
  { _traceT ∷ EitherT (State [t] e) m α
  } deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)

makeLenses ''TraceT

class MonadTrace t m | m → t where
  traceScope
    ∷ t
    → m α
    → m α

instance Functor m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t =
    traceT %~ bimapEitherT (withState (t:)) id
instance (Functor m, Monad m) ⇒ MonadError e (TraceT t e m) where
  throwError =
    TraceT . bimapEitherT return id . left
  catchError (TraceT m) h =
     lift (runEitherT m)
       >>= either (h . flip evalState []) return


data Err = Err

test ∷ TraceT Tags Err IO ()
test = do
  traceScope FrontEnd $
    traceScope GetUserInfo $ do
      liftIO $ putStrLn "ASDFADF"
      throwError Err
