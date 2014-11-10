{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Example where

import Control.Applicative
import Control.Error
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

instance Monad m ⇒ MonadTrace t (TraceT t e m) where
  traceScope t =
    traceT %~ fmapLT (withState (|> t))
instance Monad m ⇒ MonadError e (TraceT t e m) where
  throwError = TraceT . throwT . return
  catchError (TraceT m) h =
     lift (runEitherT m)
       >>= either (h . flip evalState mempty) return

class AsError m t e where
  asError
    ∷ t α
    → e
    → m α

(<?>)
  ∷ AsError m t e
  ⇒ t α
  → e
  → m α
(<?>) = asError

instance MonadError e m ⇒ AsError m Maybe e where
  asError Nothing e = throwError e
  asError (Just x) _ = return x

instance MonadError e m ⇒ AsError m (Either e') (e' → e) where
  asError (Left e) f = throwError $ f e
  asError (Right x) _ = return x

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
  fmapLT (review _ErrorTrace . flip runState mempty)
  . _traceT

test = do
  traceScope FrontEnd $
    traceScope GetUserInfo $ do
      liftIO $ putStrLn "Hello world"
      Left "asdf" <?> Err
      throwError $ Err "Damn!"

main ∷ IO ()
main =
  runTraceT test
    & eitherT (fail . show) return
--
-- OUTPUTS
--
--     Hello world
--     *** Exception: user error (FrontEnd.GetUserInfo ⇑ Err "Damn!")

