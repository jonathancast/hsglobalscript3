{-# LANGUAGE TemplateHaskell #-}
module GSI.Parser (gsposFmt, gsempty, gsempty_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gslambda, gsconstr, gsundefined)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)

gsposFmt :: GSValue
gsposFmt = $gsundefined

gsempty = $gsconstr (gsvar "empty") []

gsempty_view :: GSValue
gsempty_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "empty" ek sk e
