module GSI.RTS (Event, newEvent, await, wakeup) where

import Control.Concurrent (MVar, newMVar, newEmptyMVar, modifyMVar, takeMVar, tryPutMVar)

newtype Event = Event (MVar (Maybe [MVar ()]))

newEvent :: IO Event
newEvent = Event <$> newMVar (Just [])

await :: Event -> IO ()
await (Event mv) = do
    mb <- modifyMVar mv $ \ st -> case st of
        Nothing -> return (Nothing, Nothing)
        Just mvs -> do
            mv1 <- newEmptyMVar
            return (Just (mv1:mvs), Just mv1)
    maybe (return ()) takeMVar mb

wakeup :: Event -> IO ()
wakeup (Event mv) = do
    mvs <- modifyMVar mv $ \ st -> case st of
        Nothing -> return (Nothing, [])
        Just mvs -> return (Nothing, mvs)
    mapM_ (`tryPutMVar` ()) mvs
