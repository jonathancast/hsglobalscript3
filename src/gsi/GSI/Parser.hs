{-# LANGUAGE TemplateHaskell #-}
module GSI.Parser (gsposFmt, gsempty, gsempty_view) where

import GSI.Syn (gsvar)
import GSI.Value (GSValue, gslambda, gsconstr, gsundefined, gsav, gsae)
import GSI.ByteCode (gsbcarg, gsbclfield, gsbcundefined, gsbcconstr_view)
import GSI.String (gsbcstring)

gsposFmt :: GSValue
gsposFmt = $gslambda $ \ pos -> $gsbcarg $ \ msg ->
    $gsbclfield (gsvar "filename") pos $ \ posv ->
        $gsbcstring [ $gsav $ posv, $gsae $ $gsbcundefined ]

gsempty = $gsconstr (gsvar "empty") []

gsempty_view :: GSValue
gsempty_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ e -> $gsbcconstr_view "empty" ek sk e
