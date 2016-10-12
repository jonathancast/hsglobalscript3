{-# LANGUAGE TemplateHaskell #-}

import GSI.Value (gsundefined)
import GSI.Thread (createThread, execMainThread)

main = do
    t <- createThread $gsundefined -- =<< $gsapply gsrun [gsmain]
    execMainThread t
