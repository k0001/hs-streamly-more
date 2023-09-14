module Streamly.More.Trans
  ( unfoldRunStateT
  , unfoldEvalStateT
  , foldExceptT
  , foldRunExceptT
  , refoldExceptT
  , refoldRunExceptT
  ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import qualified Streamly.Internal.Data.Unfold as U
import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Internal.Data.Refold.Type as R

--------------------------------------------------------------------------------

-- unfoldStateT
--   :: forall z a b m
--   .  (Monad m)
--   => U.Unfold m (z, a) (z, b)
--   -> U.Unfold (StateT z m) a b
-- unfoldStateT (U.Unfold (fs0 :: s -> m (U.Step s (z, b)))
--                        (fi0 :: (z, a) -> m s)) =
--     U.Unfold fs1 fi1
--   where
--     fs1 :: s -> StateT z m (U.Step s b)
--     fs1 = \s -> StateT $ \z -> fmap (g z) (fs0 s)
--
--     fi1 :: a -> StateT z m s
--     fi1 = \a -> StateT $ \z -> do s <- fi0 (z, a)
--                                   pure (s, z)
--
--     g :: z -> U.Step s (z, b) -> (U.Step s b, z)
--     g _ (U.Yield (z, b) s) = (U.Yield b s, z) -- this is probably wrong
--     g z (U.Skip s)         = (U.Skip s,    z)
--     g z  U.Stop            = (U.Stop,      z)

unfoldRunStateT
  :: forall z a b m
  .  (Monad m)
  => m z
  -> U.Unfold (StateT z m) a b
  -> U.Unfold m a (z, b)
unfoldRunStateT mz (U.Unfold (fs0 :: s -> StateT z m (U.Step s b))
                             (fi0 :: a -> StateT z m s)) =
    U.Unfold fs1 fi1
  where
    fs1 :: (s, z) -> m (U.Step (s, z) (z, b))
    fs1 = \(s, z) -> g <$> runStateT (fs0 s) z

    fi1 :: a -> m (s, z)
    fi1 = \a -> runStateT (fi0 a) =<< mz

    g :: (U.Step s b, z) -> U.Step (s, z) (z, b)
    g (U.Yield b s, z) = U.Yield (z, b) (s, z)
    g (U.Skip s,    z) = U.Skip (s, z)
    g (U.Stop,      _) = U.Stop

unfoldEvalStateT
  :: Monad m => m s -> U.Unfold (StateT s m) a b -> U.Unfold m a b
unfoldEvalStateT = fmap (fmap (fmap snd)) unfoldRunStateT

foldStepEither :: F.Step s (Either e b) -> Either e (F.Step s b)
foldStepEither (F.Done e)    = fmap F.Done e
foldStepEither (F.Partial s) = Right (F.Partial s)

foldStepRunEither :: Either e (F.Step s b) -> F.Step s (Either e b)
foldStepRunEither (Left  e) = F.Done (Left e)
foldStepRunEither (Right x) = fmap Right x

foldExceptT
  :: Functor m => F.Fold m a (Either e b) -> F.Fold (ExceptT e m) a b
foldExceptT (F.Fold fs fi fe) = F.Fold
  (fmap (ExceptT . fmap foldStepEither) . fs)
  (ExceptT (fmap foldStepEither fi))
  (ExceptT . fe)

foldRunExceptT
  :: Functor m => F.Fold (ExceptT e m) a b -> F.Fold m a (Either e b)
foldRunExceptT (F.Fold fs fi fe) = F.Fold
  (fmap (fmap foldStepRunEither . runExceptT) . fs)
  (fmap foldStepRunEither $ runExceptT fi)
  (runExceptT . fe)

refoldExceptT
  :: Functor m => R.Refold m c a (Either e b) -> R.Refold (ExceptT e m) c a b
refoldExceptT (R.Refold fs fi fe) = R.Refold
  (fmap (ExceptT . fmap foldStepEither) . fs)
  (ExceptT . fmap foldStepEither . fi)
  (ExceptT . fe)

refoldRunExceptT
  :: Functor m => R.Refold (ExceptT e m) c a b -> R.Refold m c a (Either e b)
refoldRunExceptT (R.Refold fs fi fe) = R.Refold
  (fmap (fmap foldStepRunEither . runExceptT) . fs)
  (fmap foldStepRunEither . runExceptT . fi)
  (runExceptT . fe)

-- fromRefoldM :: forall m c a b. Monad m => m c -> R.Refold m c a b -> F.Fold m a b
-- fromRefoldM mc (R.Refold rs ri re) = F.Fold rs (ri =<< mc) re
