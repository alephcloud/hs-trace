{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
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
import Control.Lens
import Data.Monoid
import qualified Data.Sequence as S
import Data.List
import qualified Data.Foldable as F

data Tags
  = FrontEnd
  | BackEnd
  | SetPassword
  | GetUserInfo
  | Dynamo
  deriving Show

newtype TraceT t e m α
  = TraceT
  { _traceT ∷ EitherT (State (S.Seq t) e) m α
  } deriving (Functor, Monad, Applicative, MonadIO, MonadTrans)

makeLenses ''TraceT

class MonadTrace t m | m → t where
  traceScope
    ∷ t
    → m α
    → m α

instance Functor m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t =
    traceT %~ bimapEitherT (withState (|> t)) id
instance (Functor m, Monad m) ⇒ MonadError e (TraceT t e m) where
  throwError =
    TraceT . bimapEitherT return id . left
  catchError (TraceT m) h =
     lift (runEitherT m)
       >>= either (h . flip evalState mempty) return


data Err
  = Err String
  deriving Show

data ErrorTrace t e
  = ErrorTrace
  { _etError ∷ e
  , _etTrace ∷ S.Seq t
  }

instance (Show t, Show e) ⇒ Show (ErrorTrace t e) where
  showsPrec p ErrorTrace{..} =
    showParen (p > 10) $
      foldr (.) id (intersperse ('.':) $ shows <$> F.toList _etTrace)
      . (" ⇑ " ++)
      . shows _etError

makeLenses ''ErrorTrace
makePrisms ''ErrorTrace

runTraceT
  ∷ ( Functor m
    , Monad m
    )
  ⇒ TraceT t e m α
  → EitherT (ErrorTrace t e) m α
runTraceT =
  bimapEitherT (review _ErrorTrace . flip runState mempty) id
  . _traceT

test ∷ TraceT Tags Err IO ()
test = do
  traceScope FrontEnd $
    traceScope GetUserInfo $ do
      liftIO $ putStrLn "ASDFADF"
      throwError $ Err "Damn!"

main ∷ IO ()
main =
  runTraceT test
    & eitherT (fail . show) return

