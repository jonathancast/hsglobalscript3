module GSI.RTS (Event, newEvent, await, awaitAny, wakeup, OPort, newChannel, bitBucketOPort, iportReadable, tryReadIPort) where

import Control.Monad (forM_)

import Control.Concurrent (MVar, newMVar, newEmptyMVar, modifyMVar, modifyMVar_, takeMVar, readMVar, putMVar, tryPutMVar)

newtype Event = Event (MVar (Maybe [MVar ()]))

newEvent :: IO Event
newEvent = Event <$> newMVar (Just [])

occurredEvent :: IO Event
occurredEvent = Event <$> newMVar Nothing

await :: Event -> IO ()
await (Event mv) = do
    mb <- modifyMVar mv $ \ st -> case st of
        Nothing -> return (Nothing, Nothing)
        Just mvs -> do
            mv1 <- newEmptyMVar
            return (Just (mv1:mvs), Just mv1)
    maybe (return ()) takeMVar mb

awaitAny :: [Event] -> IO ()
awaitAny es = do
    mv1 <- newEmptyMVar
    forM_ es $ \ (Event mv) -> modifyMVar_ mv $ \ st -> case st of
        Nothing -> tryPutMVar mv1 () *> return Nothing
        Just mvs -> return (Just (mv1:mvs))
    takeMVar mv1

wakeup :: Event -> IO ()
wakeup (Event mv) = do
    mvs <- modifyMVar mv $ \ st -> case st of
        Nothing -> return (Nothing, [])
        Just mvs -> return (Nothing, mvs)
    mapM_ (`tryPutMVar` ()) mvs

newtype ChannelEntry a = ChannelEntry { unChannelEntry :: MVar (Either Event (a, ChannelEntry a)) }

newtype IPort a = IPort { unIPort :: MVar (ChannelEntry a) }

newtype OPort a = OPort { unOPort :: MVar (ChannelEntry a) }

newChannel :: IO (IPort a, OPort a)
newChannel = do
    ev <- newEvent
    ce <- ChannelEntry <$> newMVar (Left ev)
    ip <- IPort <$> newMVar ce
    op <- OPort <$> newMVar ce
    return (ip, op)

bitBucketOPort :: IO (OPort a)
bitBucketOPort = do
    ev <- newEvent
    ce <- ChannelEntry <$> newMVar (Left ev)
    op <- OPort <$> newMVar ce
    return op

iportReadable :: IPort a -> IO Event
iportReadable ip = do
    ce <- readMVar $ unIPort ip
    e <- readMVar $ unChannelEntry ce
    case e of
        Left ev -> return ev
        Right _ -> occurredEvent

readIPort :: IPort a -> IO a
readIPort ip = do
    ce <- takeMVar $ unIPort ip
    w ce
  where
    w ce = do
        e <- takeMVar $ unChannelEntry ce
        case e of
            Left ev -> do
                unChannelEntry ce `putMVar` e
                await ev
                w ce
            Right (x, ce1) -> do
                unIPort ip `putMVar` ce1
                return x

tryReadIPort :: IPort a -> IO (Maybe a)
tryReadIPort ip = do
    ce <- takeMVar $ unIPort ip
    e <- readMVar $ unChannelEntry ce
    case e of
        Left ev -> do
            unIPort ip `putMVar` ce
            return Nothing
        Right (x, ce1) -> do
            unIPort ip `putMVar` ce1
            return (Just x)

writeOPort :: OPort a -> a -> IO ()
writeOPort op x = do
    ce <- takeMVar $ unOPort op
    w ce
  where
    w ce = do
        e <- takeMVar $ unChannelEntry ce
        case e of
            Left ev -> do
                ev1 <- newEvent
                ce1 <- ChannelEntry <$> newMVar (Left ev1)
                unChannelEntry ce `putMVar` Right (x, ce1)
                unOPort op `putMVar` ce1
                wakeup ev
            Right (_, ce1) -> do
                unChannelEntry ce `putMVar` e
                w ce1
