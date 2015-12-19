{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.Conduit.Parser.Internal where

-- {{{ Imports
import           Control.Applicative
import           Control.Arrow             (second)
import           Control.Monad
import           Control.Monad.Catch       as Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Conduit              hiding (await, leftover)
import qualified Data.Conduit              as Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.Text                 as Text (Text, null, pack, unpack)

import           Text.Parser.Combinators   as Parser
-- }}}

-- | Core type of the package. This is basically a 'Sink' with a parsing state.
newtype ConduitParser i m a = ConduitParser (StateT (Text, [i]) (Sink i m) a)

deriving instance Applicative (ConduitParser i m)
deriving instance Functor (ConduitParser i m)
deriving instance (Monad m) => Monad (ConduitParser i m)

-- | Parsers can be combined with ('<|>'), 'some', 'many', 'optional', 'choice'.
--
-- The use of 'guard' is not recommended as it generates unhelpful error messages.
-- Please consider using 'throwM' or 'unexpected' instead.
--
-- Note: only 'ConduitParserException's will trigger the 'Alternative' features, all other exceptions are rethrown.
instance (MonadCatch m) => Alternative (ConduitParser i m) where
  empty = throwM $ Unexpected "ConduitParser.empty"

  parserA <|> parserB = catch parserA $ \(ea :: ConduitParserException) ->
    catch parserB $ \(eb :: ConduitParserException) ->
      throwM $ BothFailed ea eb

-- | Consumed elements are pushed back with 'leftover' whenever an exception occurs.
-- In other words, backtracking is supported.
instance (MonadThrow m) => MonadThrow (ConduitParser i m) where
  throwM e = case fromException (toException e) of
    Just (e' :: ConduitParserException) -> do
      backtrack
      name <- getParserName
      if Text.null name
        then ConduitParser $ throwM e'
        else ConduitParser . throwM $ NamedParserException name e'
    _ -> ConduitParser $ throwM e

instance (MonadCatch m) => MonadCatch (ConduitParser i m) where
  catch (ConduitParser f) handler = do
    buffer <- resetBuffer
    result <- ConduitParser $ Exception.try f
    case result of
      Right a -> prependBuffer buffer >> return a
      Left e -> prependBuffer buffer >> handler e

instance MonadTrans (ConduitParser i) where
  lift = ConduitParser . lift . lift

-- | Parsing combinators can be used with 'ConduitParser's.
instance (MonadCatch m) => Parsing (ConduitParser i m) where
  try parser = parser

  parser <?> name = do
    oldName <- getParserName
    setParserName $ pack name
    a <- parser
    setParserName oldName
    return a

  unexpected = throwM . Unexpected . pack

  eof = do
    result <- peek
    maybe (return ()) (const $ throwM ExpectedEndOfInput) result

  notFollowedBy parser = do
    result <- optional parser
    name <- getParserName
    forM_ result $ \_ -> throwM $ UnexpectedFollowedBy name

-- | Flipped version of ('<?>').
named :: (MonadCatch m) => Text -> ConduitParser i m a -> ConduitParser i m a
named name = flip (<?>) (unpack name)


-- | Run a 'ConduitParser'.
-- Any parsing failure will be thrown as an exception.
runConduitParser :: (MonadThrow m) => ConduitParser i m a -> Sink i m a
runConduitParser (ConduitParser p) = fst <$> runStateT p (mempty, mempty)

-- | Return the name of the parser (assigned through ('<?>')), or 'mempty' if has none.
getParserName :: ConduitParser i m Text
getParserName = ConduitParser $ gets fst

setParserName :: Text -> ConduitParser i m ()
setParserName name = ConduitParser . modify $ \(_, b) -> (name, b)

getBuffer :: ConduitParser i m [i]
getBuffer = ConduitParser $ gets snd

appendBuffer :: [i] -> ConduitParser i m ()
appendBuffer new = ConduitParser $ modify (\(n, b) -> (n, b ++ new))

prependBuffer :: [i] -> ConduitParser i m ()
prependBuffer new = ConduitParser $ modify (second (new ++))

resetBuffer :: (Monad m) => ConduitParser i m [i]
resetBuffer = do
  b <- getBuffer
  ConduitParser $ modify (\(n, _) -> (n, mempty))
  return b

backtrack :: (Monad m) => ConduitParser i m ()
backtrack = mapM_ leftover . reverse =<< resetBuffer

-- | 'Conduit.await' wrapped as a 'ConduitParser'.
--
-- If no data is available, 'UnexpectedEndOfInput' is thrown.
await :: (MonadCatch m) => ConduitParser i m i
await = do
  event <- ConduitParser . lift $ Conduit.await
  e     <- maybe (throwM UnexpectedEndOfInput) return event
  appendBuffer [e]
  return e

-- | 'Conduit.leftover' wrapped as a 'ConduitParser'.
leftover :: (Monad m) => i -> ConduitParser i m ()
leftover = ConduitParser . lift . Conduit.leftover

-- | 'Conduit.peek' wrapped as a 'ConduitParser'.
peek :: (Monad m) => ConduitParser i m (Maybe i)
peek = ConduitParser . lift $ Conduit.peek


data ConduitParserException = BothFailed ConduitParserException ConduitParserException
                            | ExpectedEndOfInput
                            | NamedParserException Text ConduitParserException
                            | UnexpectedEndOfInput
                            | UnexpectedFollowedBy Text
                            | Unexpected Text

deriving instance Eq ConduitParserException
deriving instance Show ConduitParserException

instance Exception ConduitParserException where
  displayException (BothFailed ea eb) = show ea ++ "\n" ++ show eb
  displayException ExpectedEndOfInput = "Unexpected input, expected end of input."
  displayException (NamedParserException t e) = "While parsing " ++ unpack t ++ ": " ++ show e
  displayException UnexpectedEndOfInput = "Unexpected end of input."
  displayException (UnexpectedFollowedBy t) = "Should not be followed by " ++ unpack t
  displayException (Unexpected t) = unpack t
