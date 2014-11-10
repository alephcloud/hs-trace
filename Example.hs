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
import Control.Lens hiding (Snoc)

data Void

data Tags
  = FrontEnd
  | BackEnd
  | SetPassword
  | GetUserInfo
  | Dynamo
  deriving Show


type Err t e = [t] → ([t], e)

throw
  ∷ Monad m
  ⇒ e
  → EitherT (Err t e) m a
throw  = left . flip (,)

newtype TaggedEitherT t e m α
  = TaggedEitherT
  { _taggedEitherT ∷ EitherT (Err t e) m α
  } deriving (Functor, Monad, Applicative)

instance (Functor m, Monad m) ⇒ MonadError e (TaggedEitherT t e m) where
  throwError = TaggedEitherT . bimapEitherT (flip (,)) id . left
  catchError (TaggedEitherT m) h = TaggedEitherT $ catchError m (_taggedEitherT . h . snd . ($[]))

tagScope
  ∷ Functor m
  ⇒ t
  → TaggedEitherT t e m a
  → TaggedEitherT t e m a
tagScope t = TaggedEitherT . bimapEitherT (\f ts → f (t:ts)) id . _taggedEitherT

runTaggedEitherT
  ∷ Functor m
  ⇒ TaggedEitherT t e m α
  → m (Either ([t],e) α)
runTaggedEitherT =
  fmap (_Left %~ ($ []))
  . runEitherT
  . _taggedEitherT

test ∷ TaggedEitherT Tags String IO ()
test = do
  tagScope FrontEnd $
    tagScope GetUserInfo $ do
      throwError "Damn"

