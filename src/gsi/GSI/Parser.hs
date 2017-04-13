{-# LANGUAGE TemplateHaskell #-}
module GSI.Parser (gsempty) where

import GSI.Syn (gsvar)
import GSI.Value (gsconstr)

gsempty = $gsconstr (gsvar "empty") []
