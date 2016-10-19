module GSI.RTS (Event, newEvent, wakeup) where

import Control.Concurrent (MVar, newMVar, modifyMVar, tryPutMVar)

newtype Event = Event (MVar (Maybe [MVar ()]))

newEvent :: IO Event
newEvent = Event <$> newMVar (Just [])

wakeup :: Event -> IO ()
wakeup (Event mv) = do
    mvs <- modifyMVar mv $ \ st -> case st of
        Nothing -> return (Nothing, [])
        Just mvs -> return (Nothing, mvs)
    mapM_ (`tryPutMVar` ()) mvs
