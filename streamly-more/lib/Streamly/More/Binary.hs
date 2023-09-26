module Streamly.More.Binary
  ( -- * Encoding
    encodeStream
  , encodeStreamPut
  , encodeUnfold
  , encodeUnfoldPut

    -- * Decoding
  , Error (..)
  , decodeStream
  , decodeStreamGet
  , decodeStream'
  , decodeFold
  , decodeFoldGet
  ) where

import Control.Monad
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List qualified as List
import GHC.Stack
import Numeric.Natural (Natural)
import Streamly.Data.Stream qualified as S
import Streamly.Internal.Data.Fold qualified as F
import Streamly.Internal.Data.Refold.Type qualified as R
import Streamly.Internal.Data.StreamK qualified as SK
import Streamly.Internal.Data.Unfold qualified as U

--------------------------------------------------------------------------------
-- Encoding

encodeStream
  :: forall a m
   . (Bin.Binary a, Monad m)
  => S.Stream m a
  -> S.Stream m BS.ByteString
encodeStream = encodeStreamPut Bin.put

encodeStreamPut
  :: forall a m
   . (Monad m)
  => (a -> Bin.Put)
  -> S.Stream m a
  -> S.Stream m BS.ByteString
encodeStreamPut = S.unfoldMany . encodeUnfoldPut

encodeUnfold
  :: forall a m
   . (Bin.Binary a, Applicative m)
  => U.Unfold m a BS.ByteString
encodeUnfold = encodeUnfoldPut Bin.put

encodeUnfoldPut
  :: forall a m
   . (Applicative m)
  => (a -> Bin.Put)
  -> U.Unfold m a BS.ByteString
encodeUnfoldPut p =
  U.lmap (BL.toChunks . Bin.runPut . p) (U.unfoldr List.uncons)

--------------------------------------------------------------------------------
-- Decoding

-- | A decoding error. Containing a description and the number of consumed
-- bytes.
data Error = Error Natural String
  deriving stock (Eq, Show)
  deriving anyclass (Ex.Exception)

decodeStream
  :: forall a m
   . (Bin.Binary a, Monad m, Ex.MonadThrow m)
  => S.Stream m BS.ByteString
  -> S.Stream m a
decodeStream = decodeStreamGet (pure Bin.get)

decodeStreamGet
  :: forall a m
   . (Ex.MonadThrow m)
  => m (Bin.Get a)
  -- ^ Executed for each @a@ to be parsed.
  -> S.Stream m BS.ByteString
  -> S.Stream m a
decodeStreamGet mga =
  fmap snd
    . S.morphInner (either (Ex.throwM . snd) pure <=< runExceptT)
    . decodeStream' mga

decodeStream'
  :: forall a m
   . (Monad m)
  => m (Bin.Get a)
  -- ^ Executed for each @a@ to be parsed.
  -> S.Stream m BS.ByteString
  -> S.Stream
      (ExceptT (S.Stream m BS.ByteString, Error) m)
      (Natural, a)
decodeStream' mga = curry g 0 . SK.fromStream
 where
  g
    :: (Natural, SK.StreamK m BS.ByteString)
    -> S.Stream
        (ExceptT (S.Stream m BS.ByteString, Error) m)
        (Natural, a)
  g = S.unfoldrM $ \(!n0, !sk0) -> do
    ysk0' <- lift $ fmap (uncurry SK.cons) <$> SK.uncons sk0
    forM ysk0' $ \sk0' -> do
      (res, sklo1) <- lift $ do
        ga <- mga
        SK.foldBreak (F.fromRefold decodeRefoldGet ga) sk0'
      case res of
        Right (stlo0, n1, a) -> do
          let n2 = n0 + fromIntegral n1
              sk1 = SK.fromStream stlo0 <> sklo1
          pure ((n2, a), (n2, sk1))
        Left (stlo0, n1, e) -> do
          let n2 = n0 + fromIntegral n1
              st1 = SK.toStream (SK.fromStream stlo0 <> sklo1)
          ExceptT $ pure $ Left (st1, Error n2 e)

-- | Leftovers, number of consumed bytes, and either an error message or a
-- successful parsing result.
--
-- Leftovers don't contain 'B.empty' chunks.
--
-- Note: These correspond to the 'Bin.Failure' and 'Bin.Done' constructors of
-- 'Bin.Decoder'.
type Result n a =
  Either
    (S.Stream n BS.ByteString, Bin.ByteOffset, String)
    (S.Stream n BS.ByteString, Bin.ByteOffset, a)

decodeFold
  :: forall a m n
   . (Bin.Binary a, Monad m, Monad n)
  => F.Fold m BS.ByteString (Result n a)
decodeFold = decodeFoldGet Bin.get

decodeFoldGet
  :: forall a m n
   . (Bin.Binary a, Monad m, Monad n)
  => Bin.Get a
  -> F.Fold m BS.ByteString (Result n a)
decodeFoldGet = F.fromRefold decodeRefoldGet

decodeRefoldGet
  :: forall a m n
   . (Monad m, Monad n)
  => R.Refold m (Bin.Get a) BS.ByteString (Result n a)
decodeRefoldGet =
  R.Refold
    (fmap pure . fstep)
    (pure . F.Partial . Bin.runGetIncremental)
    (pure . fextr)
 where
  fstep
    :: Bin.Decoder a
    -> BS.ByteString
    -> F.Step (Bin.Decoder a) (Result n a)
  fstep da0 bin = case da0 of
    Bin.Partial f
      | BS.null bin -> F.Partial da0
      | otherwise -> case f (Just bin) of
          Bin.Partial da1 -> F.Partial $ Bin.Partial da1
          Bin.Done blo n a -> F.Done $ Right (nonNulls [blo], n, a)
          Bin.Fail blo n e -> F.Done $ Left (nonNulls [blo], n, e)
    Bin.Done blo n a -> F.Done $ Right (nonNulls [blo, bin], n, a)
    Bin.Fail blo n e -> F.Done $ Left (nonNulls [blo, bin], n, e)

  fextr :: (HasCallStack) => Bin.Decoder a -> Result n a
  fextr = \case
    Bin.Done blo n a -> Right (nonNulls [blo], n, a)
    Bin.Fail blo n e -> Left (nonNulls [blo], n, e)
    Bin.Partial f0 -> case f0 Nothing of
      Bin.Done blo n a -> Right (nonNulls [blo], n, a)
      Bin.Fail blo n e -> Left (nonNulls [blo], n, e)
      Bin.Partial _ -> error "decodeRefoldGet: impossible"

--------------------------------------------------------------------------------
-- Misc

notNull :: BS.ByteString -> Bool
notNull = \a -> if BS.null a then False else True

nonNulls :: (Monad m) => [BS.ByteString] -> S.Stream m BS.ByteString
nonNulls = S.fromList . filter notNull
