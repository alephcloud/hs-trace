{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Monad.Trace.ErrorTrace
( ErrorTrace(..)
, _ErrorTrace
, etError
, etTrace
) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Profunctor
import Data.Sequence
import Data.List

data ErrorTrace t e
  = ErrorTrace
  { _etError ∷ !e
  , _etTrace ∷ !(Seq t)
  }

instance (Show t, Show e) ⇒ Show (ErrorTrace t e) where
  showsPrec p ErrorTrace{..} =
    showParen (p > 10) $
      foldr (.) id (intersperse ('.':) $ shows <$> F.toList _etTrace)
      . (" ⇑ " ++)
      . shows _etError

_ErrorTrace
  ∷ ( Choice p
    , Functor f
    )
  ⇒ p (ErrorTrace t e) (f (ErrorTrace t' e'))
  → p (e, Seq t) (f (e', Seq t'))
_ErrorTrace =
  dimap (uncurry ErrorTrace)
  . fmap $ \ErrorTrace{..} → (_etError, _etTrace)

etError
  ∷ Functor f
  ⇒ (e → f e')
  → ErrorTrace t e
  → f (ErrorTrace t e')
etError inj ErrorTrace{..} =
  flip ErrorTrace _etTrace
    <$> inj _etError

etTrace
  ∷ Functor f
  ⇒ (Seq t → f (Seq t'))
  → ErrorTrace t e
  → f (ErrorTrace t' e)
etTrace inj ErrorTrace{..} =
  ErrorTrace _etError
    <$> inj _etTrace
