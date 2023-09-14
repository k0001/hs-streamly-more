module Streamly.Zstd
  ( decompress
  , compress
  , Z.maxCLevel
  , Error(..)
  ) where

import Codec.Compression.Zstd.Streaming qualified as Z
import Control.Monad.Catch qualified as Ex
import Data.ByteString qualified as B
import Streamly.Data.Stream qualified as S
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | Exception that could be thrown by 'compress' or 'decompress'.
data Error = Error
  { location :: String
    -- ^ Where did the error happen in the 'compress'ion or 'decompress'ion
    -- code. This doesn't refer to the location in the input 'S.Stream'.
  , message :: String
    -- ^ Message describing the error.
  } deriving stock (Eq, Show)
    deriving anyclass (Ex.Exception)

-- | Compress a stream of 'B.ByteString' using ZSTD.
compress
  :: Ex.MonadThrow m
  => Int -- ^ ZSTD compression level. At least 1, at most 'Z.maxCLevel'.
  -> S.Stream m B.ByteString
  -> S.Stream m B.ByteString
compress = zstd . Compress

-- | Decompress a ZSTD-compressed 'S.Stream'.
decompress
  :: Ex.MonadThrow m
  => S.Stream m B.ByteString
  -> S.Stream m B.ByteString
decompress = zstd Decompress

--------------------------------------------------------------------------------
-- INTERNAL

data Op = Compress Int | Decompress

fromOp :: Op -> IO Z.Result
fromOp = \case Compress n -> Z.compress n
               Decompress -> Z.decompress

zstd
  :: forall m
  .  Ex.MonadThrow m
  => Op
  -> S.Stream m B.ByteString
  -> S.Stream m B.ByteString
zstd = \op s0 ->
    S.unfoldrM g $ Just (fromOp op, S.filter (not . B.null) s0)
  where
    g :: Maybe (IO Z.Result, S.Stream m B.ByteString)
      -> m (Maybe ( B.ByteString
                  , Maybe (IO Z.Result, S.Stream m B.ByteString) ))
    g Nothing = pure Nothing
    g (Just (ior0, s0)) =
      -- Using `unsafePerformIO` is safe because `ior0` only interacts with
      -- its internal state. By using `unsafePerformIO` here, we can avoid
      -- a `MonadIO` constraint on `m`.
      case unsafePerformIO ior0 of
        Z.Produce b ior1 -> pure $ Just (b, Just (ior1, s0))
        Z.Consume fior1 -> do
          (b, s1) <- maybe (mempty, S.nil) id <$> S.uncons s0
          g $ Just (fior1 b, s1)
        Z.Error ea eb -> Ex.throwM $ Error ea eb
        Z.Done b
          -- `Z.Done` can only happen after `mempty` was fed to a `Consumer`,
          -- which in turn can only happen if `s0` is exhausted.
          | B.null b  -> pure Nothing
          | otherwise -> pure (Just (b, Nothing))

