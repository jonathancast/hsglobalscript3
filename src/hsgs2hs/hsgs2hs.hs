{-# LANGUAGE TemplateHaskell, ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import Control.Applicative (Alternative(..))
import Control.Monad (forM)
import Control.Monad.State.Strict (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans (MonadTrans(..))

import Data.List (isSuffixOf)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exception (catchJust)
import Data.Encoding.UTF8 (UTF8(..))
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getModificationTime)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Encoding (readFile, writeFile)
import System.IO.Error (isDoesNotExistError)

import GSI.Util (Pos(..), gsfatal, fmtPos)

import HSGS.Parser (Parser, parse, string, Advanceable(..), advanceStr)
import HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Arg(..), Pattern(..), Generator(..), Param(..), interpolation, quote, scCode, eCode, qloiCode, argCode, patCode, genCode)
import HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode)

import qualified HSGS.Syntax as Syntax

main = do
    as <- getArgs
    mapM (processArg "") as

processArg m a = do
    id <- doesDirectoryExist a
    irf <- doesFileExist a
    if id then do
        as <- getDirectoryContents a
        mapM_ (\ a' -> processArg (modCat m a') (a ++ '/' : a')) $ filter (\ a' -> a' /= "." && a' /= "..") as
      else if irf && ".hsgs" `isSuffixOf` a then do
        let ?enc = UTF8Strict
        b <- needToRecompile a
        if b then do
            s <- readFile a
            case compileHSGSSource m a s of
                Left err -> do
                    hPutStrLn stderr err
                    exitWith $ ExitFailure 1
                Right s' -> do
                    writeFile (mkHSFile a) s'
        else do
            return ()
      else do
        return ()

needToRecompile a = catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)
    (do
        t0 <- getModificationTime a
        t1 <- getModificationTime (mkHSFile a)
        return $ t0 > t1
    )
    (\ _ -> return True)

modCat "" a = mkMod a
modCat m a = m ++ '.' : mkMod a

mkMod "" = ""
mkMod ".hsgs" = ""
mkMod (c:s) = c : mkMod s

compileHSGSSource :: String -> FilePath -> String -> Either String String
compileHSGSSource m fn s =
   splitInput (Pos fn 1 1) s >>=
   compileSource globalEnv m >>=
   formatOutput >>=
   return . concat . (("{-# LINE 1 " ++ show fn ++ " #-}\n"):)

mkHSFile :: FilePath -> FilePath
mkHSFile ".hsgs" = ".hs"
mkHSFile (c:s) = c:mkHSFile s
mkHSFile "" = ""

formatOutput :: [DestComp] -> Either String [String]
formatOutput (DCChar c:dcs) = ([c]:) <$> formatOutput dcs
formatOutput (DCPos pos:dcs) =
    ([ "\n", "{-# LINE " ++ show (line pos) ++ ' ' : show (filename pos) ++ " #-}\n", replicate (fromInteger (col pos - 1)) ' ' ]++) <$>
        formatOutput dcs
formatOutput (DCImports is:dcs) = (map formatImport (Set.toList is)++) <$> formatOutput dcs
formatOutput (DCExpr _ e:dcs) = ((formatExprAtom e ""):) <$> formatOutput dcs
formatOutput (dc:dcs) = $gsfatal $ "formatOutput " ++ dcCode dc ++ " next"
formatOutput [] = return []

formatImport :: HSImport -> String
formatImport (HSIType m t) = "import " ++ m ++ " (" ++ t ++ "(..))\n"
formatImport (HSIVar m v) = "import " ++ m ++ " (" ++ v ++ ")\n"
formatImport hs = $gsfatal $ "formatImport " ++ hsiCode hs ++ " next"

formatExpr :: HSExpr -> String -> String
formatExpr e@HSConstr{} = formatExprAtom e
formatExpr e@HSVar{} = formatExprAtom e
formatExpr (HSApp ef ex) = formatExpr ef . (' ':) . formatExprAtom ex
formatExpr (HSLambda vs e) = ('\\':) . foldr (.) id (map (\ v -> (' ':) . (v++)) vs) . (" -> "++) . formatExpr e
formatExpr (HSAsType e ty) = formatExpr e . (" :: "++) . (ty++)
formatExpr e = $gsfatal $ "formatExpr " ++ hsCode e ++ " next"

formatExprAtom :: HSExpr -> String -> String
formatExprAtom (HSConstr c) = (c++)
formatExprAtom (HSVar v) = (v++)
formatExprAtom (HSChar ch) = (show ch++)
formatExprAtom (HSString s) = (show s++)
formatExprAtom (HSInteger n)
    | n >= 0 = (show n++)
    | otherwise = ('(':) . (show n++) . (')':)
formatExprAtom e@HSApp{} = ('(':) . formatExpr e . (')':)
formatExprAtom e@HSLambda{} = ('(':) . formatExpr e . (')':)
formatExprAtom (HSList es) = fmt es where
    fmt [] = ("[]"++)
    fmt es = ("[ "++) . fmt' es
    fmt' [] = (" ]"++) -- Actually dead code, but keep GHC quiet
    fmt' [ e ] = formatExpr e . (" ]"++)
    fmt' (e:es) = formatExpr e . (", "++) . fmt' es
formatExprAtom e@HSAsType{} = ('(':) . formatExpr e . (')':)
formatExprAtom e = $gsfatal $ "formatExprAtom " ++ hsCode e ++ " next"

compileSource :: Env -> String -> [SourceComp] -> Either String [DestComp]
compileSource env m (SCChar c:scs) = (DCChar c:) <$> compileSource env m scs
compileSource env m (SCPos pos:scs) = (DCPos pos:) <$> compileSource env m scs
compileSource env m (SCImports:scs) = do
    dcs <- compileSource env m scs
    return $ DCImports (gatherImports m Set.empty dcs) : dcs
compileSource env m (SCDeclareVar gsv hsv:scs) = compileSource env{gsvars = Map.insert gsv (Set.empty, HSVar hsv) (gsvars env)} m scs
compileSource env m (SCDeclareView gsv hsv:scs) = compileSource env{gsviews = Map.insert gsv (Set.empty, HSVar hsv) (gsviews env)} m scs
compileSource env m (SCExpr ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileExpr (processHSVS ps env) e) <*> compileSource env m scs
compileSource env m (SCBody pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBody (processHSVS ps env) pos e) <*> compileSource env m scs
compileSource env m (SCBind pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBind (processHSVS ps env) pos e) <*> compileSource env m scs
compileSource env m (SCValue pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileValue (processHSVS ps env) pos e) <*> compileSource env m scs
compileSource env m (sc:scs) = $gsfatal $ "compileSource " ++ scCode sc ++ " next"
compileSource env m [] = return []

gatherImports :: String -> Set HSImport -> [DestComp] -> Set HSImport
gatherImports m is (DCChar _:dcs) = gatherImports m is dcs
gatherImports m is (DCPos _:dcs) = gatherImports m is dcs
gatherImports m is (DCExpr is' _:dcs) = gatherImports m (is `Set.union` Set.filter p is') dcs where
    p (HSIType m' _) = m /= m'
    p (HSIVar m' _) = m /= m'
gatherImports m is (dc:dcs) = $gsfatal $ "gatherImports " ++ dcCode dc ++ " next"
gatherImports m is [] = is

processHSVS :: [Param] -> Env -> Env
processHSVS ps env = env{
    gsvars = Map.fromList [ (v, (Set.empty, HSVar v)) | HSVSParam vs <- ps, v <- vs ] `Map.union` gsvars env
  }

processFVS :: [Param] -> Set String
processFVS ps = Set.fromList [ v | FVSParam vs <- ps, v <- vs ]

compileValue :: Env -> Pos -> Expr -> Compiler (Set HSImport, HSExpr)
compileValue env pos e@(EVar pos1 v) = case Map.lookup v (gsimplicits env) of
    Just _ -> compileThunk env pos e
    Nothing -> do
        (isv, ev) <- case Map.lookup v (gsvars env) of
            Nothing -> compileError pos1 $ v ++ " not in scope"
            Just (isv, ev) -> return (isv, ev)
        return (isv, ev)
compileValue env pos e@EGens{} = compileThunk env pos e
compileValue env pos e@EApp{} = compileThunk env pos e
compileValue env pos e = $gsfatal $ "compileValue " ++ eCode e ++ " next"

compileThunk :: Env -> Pos -> Expr -> Compiler (Set HSImport, HSExpr)
compileThunk env pos e = do
    (is, hse) <- compileExpr env e
    return (
        Set.fromList [ HSIVar "System.IO.Unsafe" "unsafePerformIO", HSIVar "GSI.Value" "gsthunk_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "unsafePerformIO" `HSApp` (HSVar "gsthunk_w" `HSApp` hspos pos `HSApp` hse)
      )

compileArg :: Env -> Pos -> Expr -> Maybe Signature -> Maybe Category -> Compiler (Set HSImport, HSExpr)
compileArg env pos e@EMissingCase{} s Nothing = compileExprToArg env pos e
compileArg env pos e@EQLO{} s Nothing = compileExprToArg env pos e
compileArg env pos (EVar pos1 v) s Nothing = case Map.lookup v (gsimplicits env) of
    Nothing -> do
        (isv, ev) <- case Map.lookup v (gsvars env) of
            Nothing -> lift $ Left $ fmtPos pos1 $ v ++ " not in scope"
            Just (isv, ev) -> return (isv, ev)
        return (
            Set.singleton (HSIType "GSI.Value" "GSArg") `Set.union` isv,
            HSConstr "GSArgVar" `HSApp` ev
          )
    Just _ -> compileExprToArg env pos (EVar pos1 v)
compileArg env pos (ENumber _ n) s Nothing = return (
    Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Value" "GSValue" ],
    HSConstr "GSArgVar" `HSApp` (HSConstr "GSNatural" `HSApp` HSInteger n)
  )
compileArg env pos (EPat p) s Nothing = lift $ compilePatArg env pos p
compileArg env pos (EPat p) s (Just Monoidal) = lift $ compileMonoidalPatArg env pos p
compileArg env pos (EOpen e) s Nothing = compileOpenArg env pos fvs e where
    fvs = case s of
        Nothing -> Set.empty
        Just (SigOpen vs) -> vs
        Just sg -> $gsfatal $ "compileArg (EOpen e) " ++ sigCode sg ++ " next"
compileArg env pos e@EGens{} s Nothing = compileExprToArg env pos e
compileArg env pos (EImpGens gs pos1) s Nothing = compileImpGensArg env pos gs pos1
compileArg env pos (EMonadGens gs pos1) (Just (SigMonad s)) Nothing = compileMonadGensArg env pos gs pos1 s
compileArg env pos (EMonadGens gs pos1) (Just s) Nothing = compileError pos $ "monadic generators with invalid signature " ++ sigCode s ++ "!"
compileArg env pos (EMonadGens gs pos1) Nothing Nothing = compileError pos "monadic generators with no signature!"
compileArg env pos e@EApp{} s Nothing = compileExprToArg env pos e
compileArg env pos e s Nothing = $gsfatal $ "compileArg " ++ eCode e ++ " next"
compileArg env pos e s (Just c) = $gsfatal $ "compileArg " ++ eCode e ++ " (Just " ++ catCode c ++ ") next"

compileExprToArg :: Env -> Pos -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileExprToArg env pos e = do
    (is, hse) <- compileExpr env e
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileExpr :: Env -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileExpr env (EMissingCase pos) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcarg_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.ByteCode" "gsbcprim_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.CalculusPrims" "gspriminsufficientcases" ],
    HSVar "gsbcarg_w" `HSApp` hspos pos `HSApp` (HSLambda ["x"] $HSVar "gsbcprim_w" `HSApp` hspos pos `HSApp` HSVar "gspriminsufficientcases" `HSApp` HSVar "x")
  )
compileExpr env (EVar pos v) = case Map.lookup v (gsimplicits env) of
    Nothing -> do
        (isv, ev) <- case Map.lookup v (gsvars env) of
            Nothing -> lift $ Left $ fmtPos pos $ v ++ " not in scope"
            Just (isv, ev) -> return (isv, ev)
        return (
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcenter_w", HSIType "GSI.Util" "Pos" ] `Set.union` isv,
            HSVar "gsbcenter_w" `HSApp` (hspos pos) `HSApp` ev
          )
    Just _ -> compileApp env (EVar pos v) []
compileExpr env (ENumber pos n) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcnatural_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcnatural_w" `HSApp` hspos pos `HSApp` HSInteger n
  )
compileExpr env (EQLO pos "qq" s) = do
    (is, as) <- w s
    return (
        Set.fromList [ HSIVar "GSI.String" "gsbcstring_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcstring_w" `HSApp` hspos pos `HSApp` HSList as
      )
  where
    w [] = return (Set.empty, [])
    w (qi@(QChar pos1 _):qis) = w_ch pos1 id (qi:qis)
    w (qi@(QQChar pos1 _):qis) = w_ch pos1 id (qi:qis)
    w (QInterpExpr pos1 e:qis) = do
        (ise, hse) <- compileArg env pos1 e Nothing Nothing
        (ist, hst) <- w qis
        return (ise `Set.union` ist, hse : hst)
    w (qi:qis) = $gsfatal $ "w " ++ qloiCode qi ++ " next"

    w_ch pos ds [] = return (string_imports, [ string_expr pos (ds "") ])
    w_ch pos ds (QChar _ ch:qis) = w_ch pos (ds . (ch:)) qis
    w_ch pos ds (QQChar _ 'n':qis) = w_ch pos (ds . ('\n':)) qis
    w_ch pos ds (QQChar _ ch:qis) | ch `elem` "§\\{}" = w_ch pos (ds . (ch:)) qis
    w_ch pos ds (QQChar _ ch:qis) = $gsfatal $ "w_ch pos ds (QQChar _ " ++ show ch ++ ":qis) next"
    w_ch pos ds qis@(QInterpExpr{}:_) = do
        (ist, hst) <- w qis
        return (string_imports `Set.union` ist, string_expr pos (ds "") : hst)
    w_ch pos ds (qi:qis) = $gsfatal $ "w_ch pos ds (" ++ qloiCode qi ++ ":qis) next"

    string_imports = Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos", HSIVar "GSI.String" "gsbcstringlit_w", HSIType "GSI.Util" "Pos" ]
    string_expr pos s = HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` (HSVar "gsbcstringlit_w" `HSApp` hspos pos `HSApp` HSString s)
compileExpr env (EQLO pos0 "log" s) = do
    (is, as) <- w s
    return (
        Set.fromList [ HSIVar "GSI.Log" "gsbclog_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbclog_w" `HSApp` hspos pos0 `HSApp` HSList as
      )
  where
    w [] = return (Set.empty, [])
    w (QChar pos1 ch:qis) = w_ch pos1 (ch:) qis
    w (QQChar pos1 ch:qis) | ch `elem` "\\" = w_ch pos1 (ch:) qis
    w (QQChar pos1 ch:qis) = $gsfatal $ "w (QQChar pos1 " ++ show ch ++ ":qis) next"
    w (QInterpExpr pos1 e:qis) = do
        (ise, hse) <- compileArg env pos1 e Nothing Nothing
        (ist, hst) <- w qis
        return (ise `Set.union` ist, hse : hst)
    w (qi:qis) = $gsfatal $ "w " ++ qloiCode qi ++ " next"

    w_ch pos1 ds [] = return (string_imports, [ string_expr pos1 (ds "") ])
    w_ch pos1 ds (QChar _ ch:qis) = w_ch pos1 (ds . (ch:)) qis
    w_ch pos1 ds (QQChar _ ch:qis) = w_ch pos1 (ds . (ch:)) qis
    w_ch pos1 ds qis@(QInterpExpr{}:_) = do
        (ist, hst) <- w qis
        return (string_imports `Set.union` ist, string_expr pos1 (ds "") : hst)
    w_ch pos1 ds (qi:qis) = $gsfatal $ "w_ch " ++ qloiCode qi ++ " next"

    string_imports = Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos", HSIVar "GSI.Log" "gsbclogstring_w", HSIType "GSI.Util" "Pos" ]
    string_expr pos s = HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` (HSVar "gsbclogstring_w" `HSApp` hspos pos `HSApp` HSString s)
compileExpr env (EQLO pos0 "r" []) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" []) next"
compileExpr env (EQLO pos0 "r" [QChar pos1 ch]) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar ch
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 'n']) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar '\n'
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 '{']) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar '{'
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 '}']) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar '}'
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 '(']) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar '('
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 ')']) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar ')'
  )
compileExpr env (EQLO pos0 "r" [QQChar pos1 ch]) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" [QQChar pos1 " ++ show ch ++ "]) next"
compileExpr env (EQLO pos0 "r" [qi]) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" [" ++ qloiCode qi ++ "]) next"
compileExpr env (EQLO pos0 "r" (qi:qis)) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" (qi:qis)) next"
compileExpr env (EQLO pos0 q s) = $gsfatal $ "compileExpr (EQLO pos " ++ show q ++ " s) next"
compileExpr env (EApp f (ArgExpr pos1 e)) = compileApp env f [(pos1, e)]
compileExpr env (EApp f (ArgField pos1 m)) = do
    (isf, hsef) <- compileArg env pos1 f Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcfield_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` isf,
        HSVar "gsbcfield_w" `HSApp` hspos pos1 `HSApp` hsef `HSApp` (HSVar "gsvar" `HSApp` HSString m)
      )
compileExpr env (EApp f a) = $gsfatal $ "compileExpr (EApp f " ++ argCode a ++ ") next"
compileExpr env (EGens gs pos1) = compileGens env gs pos1
compileExpr env e = $gsfatal $ "compileExpr " ++ eCode e ++ " next"

compileBody :: Env -> Pos -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileBody env pos e = do
    (is, hse) <- compileArg env pos e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpbody_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpbody_w" `HSApp` hspos pos `HSApp` hse
      )

compileBind :: Env -> Pos -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileBind env pos e = do
    (is, hse) <- compileArg env pos e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpbind_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpbind_w" `HSApp` hspos pos `HSApp` hse
      )

compileOpenExpr :: Env -> Pos -> Set String -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileOpenExpr env pos fvs e = do
    (is', k, env') <- compileLocalEnv env pos fvs
    (is, hse) <- compileExpr env' e
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcarg_w", HSIType "GSI.Util" "Pos" ] `Set.union` is' `Set.union` is,
        HSVar "gsbcarg_w" `HSApp` hspos pos `HSApp` k hse
      )

compileLocalEnv :: Env -> Pos -> Set String -> Compiler (Set HSImport, HSExpr -> HSExpr, Env)
compileLocalEnv env pos fvs = do
    vshsvs <- forM (Set.toList fvs) $ \ v -> do
        hsv <- getGenSym
        return (v, hsv)
    let (is, k) = foldr (\ (v, hsv) (is0, k0) -> (
                Set.fromList [ HSIVar "GSI.ByteCode" "gsbclfield_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is0,
                \ e -> HSVar "gsbclfield_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString v) `HSApp` HSVar "env" `HSApp` (HSLambda [hsv] (k0 e))
            ))
            (Set.empty, id)
            vshsvs
    return $ (is, HSLambda ["env"] . k, env{ gsvars = Map.fromList [ (v, (Set.empty, HSVar hsv)) | (v, hsv) <- vshsvs ] `Map.union` gsvars env })

compileOpenArg :: Env -> Pos -> Set String -> Expr -> Compiler (Set HSImport, HSExpr)
compileOpenArg env pos fvs e = do
    (is, hse) <- compileOpenExpr env pos fvs e
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileApp :: Env -> Expr -> [(Pos, Expr)] -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileApp env (EVar pos "value") ((_, EVar pos1 f):[]) = do
    (isf, ef) <- case Map.lookup f (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcenter_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            []
        ,
        HSVar "gsbcenter_w" `HSApp` hspos pos1 `HSApp` ef
      )
compileApp env (EVar pos "value") ((_, EVar pos1 f):as) = do
    (isf, ef) <- case Map.lookup f (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    as' <- mapM (\ ((pos2, e), s, c) -> compileArg env pos2 e s c) (zip3 as (repeat Nothing) (repeat Nothing))
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos1 `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp env (EVar pos "view") ((_, EVar pos1 v):as) = do
    (isv, ev) <- case Map.lookup v (gsviews env) of
        Nothing -> lift $ Left $ fmtPos pos $ "view " ++ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    as' <- mapM (\ ((pos2, e), s, c) -> compileArg env pos2 e s c) (zip3 as (repeat Nothing) (repeat Nothing))
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isv :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos1 `HSApp` ev `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp env (EVar pos f) as = do
    (isf, ef) <- case Map.lookup f (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    -- To implement §hs{ImHere}, we need to find the call stack for the overall application
    -- That means wrapping the whole appllication in a call to §hs{gsbcwithhere_w}
    -- Find out if we need to do that now
    let needHere = case Map.lookup f (gsimplicits env) of
            Nothing -> False
            Just ims -> any isImHere ims where
                isImHere ImHere = True
                isImHere _ = False
    let (isctxt, ctxt) = if needHere then
                (
                    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcwithhere_w", HSIType "GSI.Util" "Pos" ],
                    \ hse -> HSVar "gsbcwithhere_w" `HSApp` hspos pos `HSApp` (HSLambda ["here"] hse)
                )
            else
                (Set.empty, id)
    as0 <- case Map.lookup f (gsimplicits env) of
        Nothing -> return []
        Just ims -> forM ims $ \ im -> case im of
            ImHere -> return (Set.fromList [ HSIType "GSI.Value" "GSArg" ], HSConstr "GSArgVar" `HSApp` HSVar "here")
            _ -> $gsfatal $ "Compile implicit " ++ imCode im ++ " next"
    sig <- case Map.lookup f (gssignatures env) of
        Nothing -> return []
        Just sigM -> sigM as
    cats <- case Map.lookup f (gscategories env) of
        Nothing -> return []
        Just catsM -> catsM as
    as' <- mapM (\ ((pos1, e), s, c) -> compileArg env pos1 e s c) (zip3 as (sig ++ repeat Nothing) (cats ++ repeat Nothing))
    return (
        Set.unions $
            isctxt :
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as0 ++
            map (\ (is, _) -> is) as'
        ,
        ctxt (HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as0 ++ map (\ (_, a) -> a) as'))
      )
compileApp env (EUnary pos f) as = do
    (isf, ef) <- case Map.lookup f (gsunaries env) of
        Nothing -> lift $ Left $ fmtPos pos $ "unary " ++ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    as' <- mapM (\ (pos1, e) -> compileArg env pos1 e Nothing Nothing) as
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp env (EApp f (ArgExpr pos a)) as = compileApp env f ((pos, a):as)
compileApp env e@(EApp f (ArgField pos m)) as = do
    (isf, ef) <- compileExpr env e
    as' <- mapM (\ ((pos1, e), s, c) -> compileArg env pos1 e s c) (zip3 as (repeat Nothing) (repeat Nothing))
    return (
        Set.unions $
            Set.fromList [HSIVar "GSI.ByteCode" "gsbcapp_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapp_w" `HSApp` hspos pos `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp env (EApp f a) as = $gsfatal $ "compileApp (EApp f " ++ argCode a ++ ") next"
compileApp env f as = $gsfatal $ "compileApp " ++ eCode f ++ " next"

compileMonoidalPat :: Env -> Pattern -> Either String (Set HSImport, HSExpr)
compileMonoidalPat env p@(PVar pos _) = compileNonMonoidalPat env pos p
compileMonoidalPat env p@(PDiscard pos) = compileNonMonoidalPat env pos p
compileMonoidalPat env (PApp p0 p1) = compileMonoidalPatApp env p0 [p1]
compileMonoidalPat env p@PView{} = compileMonoidalPatApp env p []
compileMonoidalPat env p = $gsfatal $ "compileMonoidalPat " ++ patCode p ++ " next"

compileNonMonoidalPat :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compileNonMonoidalPat env pos p = do
    (isp, hsp) <- compilePat env p
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcnonmonoidalpattern_w", HSIType "GSI.Util" "Pos" ] `Set.union` isp,
        HSVar "gsbcnonmonoidalpattern_w" `HSApp` hspos pos `HSApp` hsp
      )

compileMonoidalPatArg :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compileMonoidalPatArg env pos p = do
    (is, e) <- compileMonoidalPat env p
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` e
      )

compileMonoidalPatApp :: Env -> Pattern -> [Pattern] -> Either String (Set HSImport, HSExpr)
compileMonoidalPatApp env (PView pos v) as = do
    (isv, ev) <- case Map.lookup v (gsviews env) of
        Nothing -> Left $ fmtPos pos $ "view " ++ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    as' <- mapM (compileMonoidalPat env) as
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcviewpattern_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` isv `Set.union` Set.unions (map (\ (is, _) -> is) as'),
        HSVar "gsbcviewpattern_w" `HSApp` hspos pos `HSApp` ev `HSApp` HSList (map (\ (_, e) -> HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` e) as')
      )
compileMonoidalPatApp env (PApp pf px) as = compileMonoidalPatApp env pf (px:as)
compileMonoidalPatApp env p as = $gsfatal $ "compileMonoidalPatApp " ++ patCode p ++ " next"

compilePat :: Env -> Pattern -> Either String (Set HSImport, HSExpr)
compilePat env (PVar pos v) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarpattern_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ],
    HSVar "gsbcvarpattern_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString v)
  )
compilePat env (PDiscard pos) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcdiscardpattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcdiscardpattern_w" `HSApp` hspos pos
  )
compilePat env p = $gsfatal $ "compilePat " ++ patCode p ++ " next"

compilePatArg :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compilePatArg env pos p = do
    (is, e) <- compilePat env p
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` e
      )

compileMonadGensArg :: Env -> Pos -> [(Pos, Generator)] -> Pos -> SigMonad -> Compiler (Set HSImport, HSExpr)
compileMonadGensArg env pos gs pos1 s = do
    (is, hse) <- compileMonadGens env gs pos1 s
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileImpGensArg :: Env -> Pos -> [(Pos, Generator)] -> Pos -> Compiler (Set HSImport, HSExpr)
compileImpGensArg env pos gs pos1 = do
    (is, hse) <- compileImpGens env gs pos1
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileMonadGens :: Env -> [(Pos, Generator)] -> Pos -> SigMonad -> Compiler (Set HSImport, HSExpr)
compileMonadGens env ((pos, g):gs) pos1 s = do
    let (bindis, bindhse) = gsbind s
    let (unitis, unithse) = gsunit s
    (is, hse) <- compileMonadGenArg env pos g s
    (ist, hst) <- compileOpenMonadGens env (genBoundVars g) gs pos1 s
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposemonadgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` bindis `Set.union` unitis `Set.union` is `Set.union` ist,
        HSVar "gsbccomposemonadgen_w" `HSApp` hspos pos `HSApp` (HSConstr "GSArgVar" `HSApp` bindhse) `HSApp` (HSConstr "GSArgVar" `HSApp` unithse) `HSApp` hse `HSApp` hst
      )
compileMonadGens env [] pos1 s = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptymonadgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` unitis,
    HSVar "gsbcemptymonadgen_w" `HSApp` hspos pos1 `HSApp` (HSConstr "GSArgVar" `HSApp` unithse)
  ) where
    (unitis, unithse) = gsunit s

compileOpenMonadGens :: Env -> Set String -> [(Pos, Generator)] -> Pos -> SigMonad -> Compiler (Set HSImport, HSExpr)
compileOpenMonadGens env fvs gs pos1 s = do
    -- If we have generators, compile them in the environment with §hs{fvs} in scope.
    -- Otherwise, compile them in env because we have nothing to do.
    (is', k, env') <- case gs of
        ((pos, g):_) -> compileLocalEnv env pos fvs
        [] -> return (Set.empty, HSLambda ["env"], env)
    (is, hse) <- compileMonadGens env' gs pos1 s
    return (is' `Set.union` is, k hse)

compileMonadGenArg :: Env -> Pos -> Generator -> SigMonad -> Compiler (Set HSImport, HSExpr)
compileMonadGenArg env pos g s = do
    (is, hse) <- compileMonadGen env pos g s
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileMonadGen :: Env -> Pos -> Generator -> SigMonad -> Compiler (Set HSImport, HSExpr)
compileMonadGen env pos (ExecGenerator pos1 e) s = do
    let (mapis, maphse) = gsmap s
    (is, hse) <- compileArg env pos1 e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcexecgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` mapis `Set.union` is,
        HSVar "gsbcexecgen_w" `HSApp` hspos pos `HSApp` (HSConstr "GSArgVar" `HSApp` maphse) `HSApp` hse
      )
compileMonadGen env pos (BindGenerator x pos1 e) s = do
    let (mapis, maphse) = gsmap s
    (is, hse) <- compileArg env pos1 e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarbind_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg", HSIVar "GSI.Syn" "gsvar" ] `Set.union` mapis `Set.union` is,
        HSVar "gsbcvarbind_w" `HSApp` hspos pos `HSApp` (HSConstr "GSArgVar" `HSApp` maphse) `HSApp` (HSVar "gsvar" `HSApp` HSString x) `HSApp` hse
      )
compileMonadGen env pos g s = $gsfatal $ "compileMonadGen env pos " ++ genCode g ++ " s next"

compileGens :: Env -> [(Pos, Generator)] -> Pos -> Compiler (Set HSImport, HSExpr)
compileGens env ((pos, g):gs) pos1 = do
    (is, hse) <- compileGenArg env pos g
    (ist, hst) <- compileGens env gs pos1
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposegen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` is `Set.union` ist,
        HSVar "gsbccomposegen_w" `HSApp` hspos pos `HSApp` hse `HSApp` (HSLambda ["env"] hst)
      )
compileGens env [] pos1 = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptygen_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcemptygen_w" `HSApp` hspos pos1
  )

compileGenArg :: Env -> Pos -> Generator -> Compiler (Set HSImport, HSExpr)
compileGenArg env pos g = do
    (is, hse) <- compileGen env pos g
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileGen :: Env -> Pos -> Generator -> Compiler (Set HSImport, HSExpr)
compileGen env pos (MatchGenerator v pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarmatch_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is,
        HSVar "gsbcvarmatch_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString v) `HSApp` hse
      )
compileGen env pos g = $gsfatal $ "compileGen env pos " ++ genCode g ++ " next"

compileImpGens :: Env -> [(Pos, Generator)] -> Pos -> Compiler (Set HSImport, HSExpr)
compileImpGens env ((pos, g):gs) pos1 = do
    (is, hse) <- compileImpGenArg env pos g
    (ist, hst) <- compileOpenImpGens env (genBoundVars g) gs pos1
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposeimpgen_w", HSIType "GSI.Util" "Pos" ] `Set.union` is `Set.union` ist,
        HSVar "gsbccomposeimpgen_w" `HSApp` hspos pos `HSApp` hse `HSApp` hst
      )
compileImpGens env [] pos1 = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptyimpgen_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcemptyimpgen_w" `HSApp` hspos pos1
  )

compileOpenImpGens :: Env -> Set String -> [(Pos, Generator)] -> Pos -> Compiler (Set HSImport, HSExpr)
compileOpenImpGens env fvs gs pos1 = do
    -- If we have generators, compile them in the environment with §hs{fvs} in scope.
    -- Otherwise, compile them in env because we have nothing to do.
    (is', k, env') <- case gs of
        ((pos, g):_) -> compileLocalEnv env pos fvs
        [] -> return (Set.empty, HSLambda ["env"], env)
    (is, hse) <- compileImpGens env' gs pos1
    return (is' `Set.union` is, k hse)

compileImpGenArg :: Env -> Pos -> Generator -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileImpGenArg env pos g = do
    (is, hse) <- compileImpGen env pos g
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileImpGen :: Env -> Pos -> Generator -> Compiler (Set HSImport, HSExpr)
compileImpGen env pos (ExecGenerator pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpexecbind_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpexecbind_w" `HSApp` hspos pos1 `HSApp` hse
      )
compileImpGen env pos (BindGenerator x pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpvarbind_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is,
        HSVar "gsbcimpvarbind_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString x) `HSApp` hse
      )
compileImpGen env pos g = $gsfatal $ "compileImpGen env pos " ++ genCode g ++ " next"

hspos :: Pos -> HSExpr
hspos pos = HSConstr "Pos" `HSApp` HSString (filename pos) `HSApp` HSInteger (line pos) `HSApp` HSInteger (col pos)

splitInput :: Pos -> String -> Either String [SourceComp]
splitInput pos ('$':s) = case parse interpolation pos s of
    Left err -> (SCChar '$':) <$> splitInput (advance '$' pos) s
    Right (r, pos', s') -> ((r:) . (SCPos pos':)) <$> splitInput pos' s'
splitInput pos ('[':'g':'s':':':s) = case parse (quote Syntax.globalEnv pos <* string "|]") (advanceStr "[gs:" pos) s of
    Left err -> error err
    Right (r, pos', s') -> ((r:) . (SCPos pos':)) <$> splitInput pos' s'
splitInput pos (c:s) = (SCChar c:) <$> splitInput (advance c pos) s
splitInput pos "" = return []

type Compiler = StateT Integer (Either String)

getGenSym :: Compiler String
getGenSym = state $ \ n -> ("x" ++ show n, n + 1)

runCompiler :: StateT Integer (Either String) a -> Either String a
runCompiler a = evalStateT a 0

compileError :: Pos -> String -> Compiler a
compileError pos msg = lift $ Left $ fmtPos pos msg

globalEnv :: Env
globalEnv = Env{
    gsvars = Map.fromList [
        ("$", (Set.singleton $ HSIVar "GSI.StdLib" "gsapply_fn", HSVar "gsapply_fn")),
        ("&&", (Set.singleton $ HSIVar "GSI.Bool" "gsshort_circuit_and", HSVar "gsshort_circuit_and")),
        ("*", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_multiply", HSVar "gsnatural_multiply")),
        ("*>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_then", HSVar "gsparser_then")),
        ("+", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_add", HSVar "gsnatural_add")),
        ("-", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_subtract", HSVar "gsnatural_subtract")),
        ("-?", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_subtract_maybe", HSVar "gsnatural_subtract_maybe")),
        (":", (Set.singleton $ HSIVar "GSI.List" "gscons", HSVar "gscons")),
        ("<*", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_after", HSVar "gsparser_after")),
        ("<*>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_app", HSVar "gsparser_app")),
        ("<>", (Set.singleton $ HSIVar "GSI.List" "gsappend", HSVar "gsappend")),
        ("<|>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_or", HSVar "gsparser_or")),
        ("addition.+", (Set.singleton $ HSIVar "GSI.Addition" "gsaddition_plus", HSVar "gsaddition_plus")),
        ("advance-rune", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_advanceRune", HSVar "gsparser_advanceRune")),
        ("analyze", (Set.singleton $ HSIVar "GSI.StdLib" "gsanalyze", HSVar "gsanalyze")),
        ("analyze-impM", (Set.singleton $ HSIVar "GSI.StdLib" "gsanalyzeImpM", HSVar "gsanalyzeImpM")),
        ("analyze-type-checker-M", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_analyze", HSVar "gstype_checker_analyze")),
        ("annotate-document", (Set.singleton $ HSIVar "GSDL.Annotate" "gsannotate_document", HSVar "gsannotate_document")),
        ("annotate-qloitem", (Set.singleton $ HSIVar "GSDL.Annotate" "gsannotate_qloitem", HSVar "gsannotate_qloitem")),
        ("annotator.env.category.get", (Set.singleton $ HSIVar "GSDL.Annotator" "gsannotator_env_category_get", HSVar "gsannotator_env_category_get")),
        ("annotator.env.category.insert", (Set.singleton $ HSIVar "GSDL.Annotator" "gsannotator_env_category_insert", HSVar "gsannotator_env_category_insert")),
        ("annotator.env.empty", (Set.singleton $ HSIVar "GSDL.Annotator" "gsannotator_env_empty", HSVar "gsannotator_env_empty")),
        ("annotator.env.namespace.get", (Set.singleton $ HSIVar "GSDL.Annotator" "gsannotator_env_namespace_get", HSVar "gsannotator_env_namespace_get")),
        ("annotator.env.namespace.insert", (Set.singleton $ HSIVar "GSDL.Annotator" "gsannotator_env_namespace_insert", HSVar "gsannotator_env_namespace_insert")),
        ("arg.explicit", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_explicit", HSVar "gsarg_explicit")),
        ("arg.expr", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_expr", HSVar "gsarg_expr")),
        ("arg.here", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_here", HSVar "gsarg_here")),
        ("case", (Set.singleton $ HSIVar "GSI.StdLib" "gscase", HSVar "gscase")),
        ("category.monoidal", (Set.singleton $ HSIVar "GSDL.AST" "gscategory_monoidal", HSVar "gscategory_monoidal")),
        ("char", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_char", HSVar "gsparser_char")),
        ("compile-document", (Set.singleton $ HSIVar "GSDL.Compile" "gscompile_document", HSVar "gscompile_document")),
        ("compiler.env.empty", (Set.singleton $ HSIVar "GSDL.Compiler" "gscompiler_env_empty", HSVar "gscompiler_env_empty")),
        ("compiler.env.value.get", (Set.singleton $ HSIVar "GSDL.Compiler" "gscompiler_env_value_get", HSVar "gscompiler_env_value_get")),
        ("compiler.env.value.insert", (Set.singleton $ HSIVar "GSDL.Compiler" "gscompiler_env_value_insert", HSVar "gscompiler_env_value_insert")),
        ("compiler.env.view.get", (Set.singleton $ HSIVar "GSDL.Compiler" "gscompiler_env_view_get", HSVar "gscompiler_env_view_get")),
        ("compiler.env.view.insert", (Set.singleton $ HSIVar "GSDL.Compiler" "gscompiler_env_view_insert", HSVar "gscompiler_env_view_insert")),
        ("concat", (Set.singleton $ HSIVar "GSI.List" "gsconcat", HSVar "gsconcat")),
        ("create-thread", (Set.singleton $ HSIVar "GSI.GSI" "gsicreateThread", HSVar "gsicreateThread")),
        ("display-rune", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_displayRune", HSVar "gsparser_displayRune")),
        ("document.in", (Set.singleton $ HSIVar "GSDL.AST" "gsdocument_in", HSVar "gsdocument_in")),
        ("drop", (Set.singleton $ HSIVar "GSI.List" "gsdrop", HSVar "gsdrop")),
        ("either.>>=", (Set.singleton $ HSIVar "GSI.Either" "gseitherbind", HSVar "gseitherbind")),
        ("either.for", (Set.singleton $ HSIVar "GSI.Either" "gseitherFor", HSVar "gseitherFor")),
        ("either.mapM", (Set.singleton $ HSIVar "GSI.Either" "gseither_mapM", HSVar "gseither_mapM")),
        ("empty", (Set.singleton $ HSIVar "GSI.Parser" "gsempty", HSVar "gsempty")),
        ("error", (Set.singleton $ HSIVar "GSI.StdLib" "gserror", HSVar "gserror")),
        ("exec-main-thread", (Set.singleton $ HSIVar "GSI.GSI" "gsiexecMainThread", HSVar "gsiexecMainThread")),
        ("expr", (Set.singleton $ HSIVar "GSDL.Syntax" "expr", HSVar "expr")),
        ("expr.app", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_app", HSVar "gsexpr_app")),
        ("expr.atom", (Set.singleton $ HSIVar "GSDL.Syntax" "gsexpr_atom", HSVar "gsexpr_atom")),
        ("expr.missing-case", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_missing_case", HSVar "gsexpr_missing_case")),
        ("expr.monoidal-pat", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_monoidal_pat", HSVar "gsexpr_monoidal_pat")),
        ("expr.natural", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_natural", HSVar "gsexpr_natural")),
        ("expr.open-expr", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_open_expr", HSVar "gsexpr_open_expr")),
        ("expr.pat", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_pat", HSVar "gsexpr_pat")),
        ("expr.qlo", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_qlo", HSVar "gsexpr_qlo")),
        ("expr.var", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_var", HSVar "gsexpr_var")),
        ("false", (Set.singleton $ HSIVar "GSI.Bool" "gsfalse", HSVar "gsfalse")),
        ("file", (Set.singleton $ HSIVar "GSDL.Syntax" "file", HSVar "file")),
        ("file.document", (Set.singleton $ HSIVar "GSDL.AST" "gsfile_document", HSVar "gsfile_document")),
        ("file.read", (Set.singleton $ HSIVar "GSI.Env" "gsfileRead", HSVar "gsfileRead")),
        ("file.stat", (Set.singleton $ HSIVar "GSI.Env" "gsfileStat", HSVar "gsfileStat")),
        ("filter", (Set.singleton $ HSIVar "GSI.List" "gsfilter", HSVar "gsfilter")),
        ("fmtdecimal", (Set.singleton $ HSIVar "GSI.String" "gsfmtdecimal", HSVar "gsfmtdecimal")),
        ("foldl", (Set.singleton $ HSIVar "GSI.List" "gsfoldl", HSVar "gsfoldl")),
        ("foldr", (Set.singleton $ HSIVar "GSI.List" "gsfoldr", HSVar "gsfoldr")),
        ("for", (Set.singleton $ HSIVar "GSI.StdLib" "gsfor", HSVar "gsfor")),
        ("get-pos", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_getPos", HSVar "gsparser_getPos")),
        ("gsae", (Set.singleton $ HSIVar "GSI.GSI" "gsigsae", HSVar "gsigsae")),
        ("gsapply", (Set.singleton $ HSIVar "GSI.GSI" "gsigsapply", HSVar "gsigsapply")),
        ("gsav", (Set.singleton $ HSIVar "GSI.GSI" "gsigsav", HSVar "gsigsav")),
        ("gsbcapply", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcapply", HSVar "gsigsbcapply")),
        ("gsbcarg", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcarg", HSVar "gsigsbcarg")),
        ("gsbcdiscardpattern", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcdiscardpattern", HSVar "gsigsbcdiscardpattern")),
        ("gsbcenter", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcenter", HSVar "gsigsbcenter")),
        ("gsbcinsufficientcases", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcinsufficientcases", HSVar "gsigsbcinsufficientcases")),
        ("gsbclfield", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbclfield", HSVar "gsigsbclfield")),
        ("gsbcnatural", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcnatural", HSVar "gsigsbcnatural")),
        ("gsbcnonmonoidalpattern", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcnonmonoidalpattern", HSVar "gsigsbcnonmonoidalpattern")),
        ("gsbcundefined", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcundefined", HSVar "gsigsbcundefined")),
        ("gsbcvarpattern", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcvarpattern", HSVar "gsigsbcvarpattern")),
        ("gsbcviewpattern", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcviewpattern", HSVar "gsigsbcviewpattern")),
        ("gsbcwithhere", (Set.singleton $ HSIVar "GSI.GSI" "gsigsbcwithhere", HSVar "gsigsbcwithhere")),
        ("gseval-sync", (Set.singleton $ HSIVar "GSI.GSI" "gsigsevalSync", HSVar "gsigsevalSync")),
        ("gsfmt-error", (Set.singleton $ HSIVar "GSI.GSI" "gsigsfmtError", HSVar "gsigsfmtError")),
        ("gsi.monad", (Set.singleton $ HSIVar "GSI.GSI" "gsi_monad", HSVar "gsi_monad")),
        ("gsinject", (Set.singleton $ HSIVar "GSI.GSI" "gsigsinject", HSVar "gsigsinject")),
        ("gsio.monad", (Set.singleton $ HSIVar "GSI.GSIO" "gsio_monad", HSVar "gsio_monad")),
        ("gsmain", (Set.singleton $ HSIVar "GSI.Main" "gsmain", HSVar "gsmain")),
        ("gsthunk", (Set.singleton $ HSIVar "GSI.GSI" "gsigsthunk", HSVar "gsigsthunk")),
        ("gsundefined", (Set.singleton $ HSIVar "GSI.GSI" "gsigsundefined", HSVar "gsigsundefined")),
        ("gsv", (Set.singleton $ HSIVar "GSI.Log" "gsloggsv", HSVar "gsloggsv")),
        ("gsvalue.constr", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_constr", HSVar "gsvalue_constr")),
        ("gsvar", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar", HSVar "gsigsvar")),
        ("gsvar.compare", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar_compare", HSVar "gsigsvar_compare")),
        ("gsvar.fmt-atom", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar_fmtAtom", HSVar "gsigsvar_fmtAtom")),
        ("gsvar.fmt-bind-atom", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar_fmtBindAtom", HSVar "gsigsvar_fmtBindAtom")),
        ("gsvar.name", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar_name", HSVar "gsigsvar_name")),
        ("gsvar.≡", (Set.singleton $ HSIVar "GSI.GSI" "gsigsvar_eq", HSVar "gsigsvar_eq")),
        ("ident", (Set.singleton $ HSIVar "GSDL.Syntax" "ident", HSVar "ident")),
        ("impfor", (Set.singleton $ HSIVar "GSI.StdLib" "gsimpfor", HSVar "gsimpfor")),
        ("implicit.here", (Set.singleton $ HSIVar "GSDL.AST" "gsimplicit_here", HSVar "gsimplicit_here")),
        ("impunit", (Set.singleton $ HSIVar "GSI.StdLib" "gsimpunit", HSVar "gsimpunit")),
        ("instantiate", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_instantiate", HSVar "gstype_checker_instantiate")),
        ("is-ascii-digit", (Set.singleton $ HSIVar "GSI.Rune" "gsisAsciiDigit", HSVar "gsisAsciiDigit")),
        ("is-lower", (Set.singleton $ HSIVar "GSI.Rune" "gsisLower", HSVar "gsisLower")),
        ("is-space", (Set.singleton $ HSIVar "GSI.Rune" "gsisSpace", HSVar "gsisSpace")),
        ("is-upper", (Set.singleton $ HSIVar "GSI.Rune" "gsis_upper", HSVar "gsis_upper")),
        ("just", (Set.singleton $ HSIVar "GSI.Maybe" "gsjust", HSVar "gsjust")),
        ("kind.pointed", (Set.singleton $ HSIVar "GSDL.AST" "gskind_pointed", HSVar "gskind_pointed")),
        ("left", (Set.singleton $ HSIVar "GSI.Either" "gsleft", HSVar "gsleft")),
        ("lexeme", (Set.singleton $ HSIVar "GSDL.Syntax" "lexeme", HSVar "lexeme")),
        ("list.∈", (Set.singleton $ HSIVar "GSI.List" "gslist_member", HSVar "gslist_member")),
        ("list.∉", (Set.singleton $ HSIVar "GSI.List" "gslist_not_member", HSVar "gslist_not_member")),
        ("log.<>", (Set.singleton $ HSIVar "GSI.StdLib" "gscompose", HSVar "gscompose")),
        ("log.char", (Set.singleton $ HSIVar "GSI.Log" "gslogchar", HSVar "gslogchar")),
        ("log.dstr", (Set.singleton $ HSIVar "GSI.Log" "gslog_dstr", HSVar "gslog_dstr")),
        ("log.fmt", (Set.singleton $ HSIVar "GSI.Log" "gslog_fmt", HSVar "gslog_fmt")),
        ("log.nil", (Set.singleton $ HSIVar "GSI.Log" "gslogempty", HSVar "gslogempty")),
        ("log.str", (Set.singleton $ HSIVar "GSI.Log" "gslog_str", HSVar "gslog_str")),
        ("many", (Set.singleton $ HSIVar "GSI.Parser" "gsmany", HSVar "gsmany")),
        ("many1", (Set.singleton $ HSIVar "GSI.Parser" "gsmany1", HSVar "gsmany1")),
        ("map", (Set.singleton $ HSIVar "GSI.List" "gsmap", HSVar "gsmap")),
        ("map2", (Set.singleton $ HSIVar "GSI.List" "gsmap2", HSVar "gsmap2")),
        ("map3", (Set.singleton $ HSIVar "GSI.List" "gsmap3", HSVar "gsmap3")),
        ("matching", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_matching", HSVar "gsparser_matching")),
        ("maybe", (Set.singleton $ HSIVar "GSI.Maybe" "gsmaybe", HSVar "gsmaybe")),
        ("monad.>>=", (Set.singleton $ HSIVar "GSI.Monad" "gsmonad_bind", HSVar "gsmonad_bind")),
        ("natural.addition", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_addition", HSVar "gsnatural_addition")),
        ("natural.div-mod", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_div_mod", HSVar "gsnatural_div_mod")),
        ("natural.fmt-decimal", (Set.singleton $ HSIVar "GSI.Format" "gsnatural_fmt_decimal", HSVar "gsnatural_fmt_decimal")),
        ("natural.read-decimal", (Set.singleton $ HSIVar "GSI.Format" "gsnatural_read_decimal", HSVar "gsnatural_read_decimal")),
        ("natural.≡", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_eq", HSVar "gsnatural_eq")),
        ("nil", (Set.singleton $ HSIVar "GSI.List" "gsnil", HSVar "gsnil")),
        ("not", (Set.singleton $ HSIVar "GSI.Bool" "gsnot", HSVar "gsnot")),
        ("not-followed-by", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_notFollowedBy", HSVar "gsparser_notFollowedBy")),
        ("nothing", (Set.singleton $ HSIVar "GSI.Maybe" "gsnothing", HSVar "gsnothing")),
        ("optional", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_optional", HSVar "gsparser_optional")),
        ("order-of-being.open-expr", (Set.singleton $ HSIVar "GSDL.AST" "gsorder_of_being_open_expr", HSVar "gsorder_of_being_open_expr")),
        ("order-of-being.pattern", (Set.singleton $ HSIVar "GSDL.AST" "gsorder_of_being_pattern", HSVar "gsorder_of_being_pattern")),
        ("ordered-map.empty", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_empty", HSVar "gsordered_map_empty")),
        ("ordered-map.get", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_get", HSVar "gsordered_map_get")),
        ("ordered-map.insert", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_insert", HSVar "gsordered_map_insert")),
        ("ordered-map.keys", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_keys", HSVar "gsordered_map_keys")),
        ("ordered-map.to-list", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_to_list", HSVar "gsordered_map_to_list")),
        ("ordered-map.values", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_values", HSVar "gsordered_map_values")),
        ("ordered-map.∪", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_union", HSVar "gsordered_map_union")),
        ("ordered-set.empty", (Set.singleton $ HSIVar "GSI.Set" "gsordered_set_empty", HSVar "gsordered_set_empty")),
        ("ordered-set.insert", (Set.singleton $ HSIVar "GSI.Set" "gsordered_set_insert", HSVar "gsordered_set_insert")),
        ("ordered-set.∈", (Set.singleton $ HSIVar "GSI.Set" "gsordered_set_member", HSVar "gsordered_set_member")),
        ("parse", (Set.singleton $ HSIVar "GSI.Parser" "gsparse", HSVar "gsparse")),
        ("parser-run", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_run", HSVar "gsparser_run")),
        ("parser.>>=", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_bind", HSVar "gsparser_bind")),
        ("parser.choice", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_choice", HSVar "gsparser_choice")),
        ("parser.env.else.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_else_get", HSVar "gsdl_parser_env_else_get")),
        ("parser.env.else.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_else_insert", HSVar "gsdl_parser_env_else_insert")),
        ("parser.env.empty", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_empty", HSVar "gsdl_parser_env_empty")),
        ("parser.env.missing-body.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_missing_body_get", HSVar "gsdl_parser_env_missing_body_get")),
        ("parser.env.missing-body.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_missing_body_insert", HSVar "gsdl_parser_env_missing_body_insert")),
        ("parser.env.missing-else.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_missing_else_get", HSVar "gsdl_parser_env_missing_else_get")),
        ("parser.env.missing-else.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_missing_else_insert", HSVar "gsdl_parser_env_missing_else_insert")),
        ("parser.env.qlo.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_qlo_get", HSVar "gsdl_parser_env_qlo_get")),
        ("parser.env.qlo.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_qlo_insert", HSVar "gsdl_parser_env_qlo_insert")),
        ("parser.env.signature.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_signature_get", HSVar "gsdl_parser_env_signature_get")),
        ("parser.env.signature.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_signature_insert", HSVar "gsdl_parser_env_signature_insert")),
        ("parser.env.syntactic-categories", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_syntactic_categories", HSVar "gsdl_parser_env_syntactic_categories")),
        ("parser.env.syntactic-category.get", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_syntactic_category_get", HSVar "gsdl_parser_env_syntactic_category_get")),
        ("parser.env.syntactic-category.insert", (Set.singleton $ HSIVar "GSDL.Parser" "gsdl_parser_env_syntactic_category_insert", HSVar "gsdl_parser_env_syntactic_category_insert")),
        ("parser.eof", (Set.singleton $ HSIVar "GSI.Parser" "gseof", HSVar "gseof")),
        ("parser.fail", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_fail", HSVar "gsparser_fail")),
        ("parser.for", (Set.singleton $ HSIVar "GSI.Parser" "gsparserFor", HSVar "gsparserFor")),
        ("parser.map", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_map", HSVar "gsparser_map")),
        ("parser.map2", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_map2", HSVar "gsparser_map2")),
        ("parser.unit", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unit", HSVar "gsparser_unit")),
        ("pat.app", (Set.singleton $ HSIVar "GSDL.AST" "gspat_app", HSVar "gspat_app")),
        ("pat.bound-vars", (Set.singleton $ HSIVar "GSDL.Annotator" "gspat_bound_vars", HSVar "gspat_bound_vars")),
        ("pat.discard", (Set.singleton $ HSIVar "GSDL.AST" "gspat_discard", HSVar "gspat_discard")),
        ("pat.var", (Set.singleton $ HSIVar "GSDL.AST" "gspat_var", HSVar "gspat_var")),
        ("pat.view", (Set.singleton $ HSIVar "GSDL.AST" "gspat_view", HSVar "gspat_view")),
        ("pos.fmt", (Set.singleton $ HSIVar "GSI.Parser" "gsposFmt", HSVar "gsposFmt")),
        ("pragma.whitespace", (Set.singleton $ HSIVar "GSDL.Syntax" "pragmawhitespace", HSVar "pragmawhitespace")),
        ("pragmas", (Set.singleton $ HSIVar "GSDL.Syntax" "pragmas", HSVar "pragmas")),
        ("print", (Set.singleton $ HSIVar "GSI.Env" "gsprint", HSVar "gsprint")),
        ("print-error", (Set.singleton $ HSIVar "GSI.Env" "gsprintError", HSVar "gsprintError")),
        ("print-rune", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_print_rune", HSVar "gsparser_print_rune")),
        ("qloitem.char", (Set.singleton $ HSIVar "GSDL.AST" "gsqloitem_char", HSVar "gsqloitem_char")),
        ("repeat", (Set.singleton $ HSIVar "GSI.List" "gsrepeat", HSVar "gsrepeat")),
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright", HSVar "gsright")),
        ("rune.code-point", (Set.singleton $ HSIVar "GSI.Rune" "gsrune_code_point", HSVar "gsrune_code_point")),
        ("rune.from-code-point", (Set.singleton $ HSIVar "GSI.Rune" "gsrune_from_code_point", HSVar "gsrune_from_code_point")),
        ("rune.≠", (Set.singleton $ HSIVar "GSI.Rune" "gsruneEq", HSVar "gsrune_neq")),
        ("rune.≡", (Set.singleton $ HSIVar "GSI.Rune" "gsruneEq", HSVar "gsruneEq")),
        ("st.get-var", (Set.singleton $ HSIVar "GSI.ST" "gsstgetvar", HSVar "gsstgetvar")),
        ("st.ref.new", (Set.singleton $ HSIVar "GSI.ST" "gsstrefnew", HSVar "gsstrefnew")),
        ("st.ref.≡", (Set.singleton $ HSIVar "GSI.ST" "gsstrefeq", HSVar "gsstrefeq")),
        ("st.run", (Set.singleton $ HSIVar "GSI.ST" "gsstrun", HSVar "gsstrun")),
        ("st.set-var", (Set.singleton $ HSIVar "GSI.ST" "gsstsetvar", HSVar "gsstsetvar")),
        ("string", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_string", HSVar "gsparser_string")),
        ("symbol", (Set.singleton $ HSIVar "GSI.Parser" "gssymbol", HSVar "gssymbol")),
        ("syntactic-category.lambda-like", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_lambda_like", HSVar "gssyntactic_category_lambda_like")),
        ("syntactic-category.variable", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_variable", HSVar "gssyntactic_category_variable")),
        ("syntactic-category.where", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_where", HSVar "gssyntactic_category_where")),
        ("true", (Set.singleton $ HSIVar "GSI.Bool" "gstrue", HSVar "gstrue")),
        ("type-check-arg", (Set.singleton $ HSIVar "GSDL.TypeCheck" "gstype_check_arg", HSVar "gstype_check_arg")),
        ("type-check-document", (Set.singleton $ HSIVar "GSDL.TypeCheck" "gstype_check_document", HSVar "gstype_check_document")),
        ("type-check-expr", (Set.singleton $ HSIVar "GSDL.TypeCheck" "gstype_check_expr", HSVar "gstype_check_expr")),
        ("type-check-implicit", (Set.singleton $ HSIVar "GSDL.TypeCheck" "gstype_check_implicit", HSVar "gstype_check_implicit")),
        ("type-checker.>>=", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_bind", HSVar "gstype_checker_bind")),
        ("type-checker.env.get-implicit", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_env_get_implicit", HSVar "gstype_checker_env_get_implicit")),
        ("type-checker.env.get-type", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_env_get_type", HSVar "gstype_checker_env_get_type")),
        ("type-checker.env.get-view-type", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_env_get_view_type", HSVar "gstype_checker_env_get_view_type")),
        ("type-checker.fmt-type", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_fmt_type", HSVar "gstype_checker_fmt_type")),
        ("type-checker.foldM", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_foldM", HSVar "gstype_checker_foldM")),
        ("type-checker.for", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_for", HSVar "gstype_checker_for")),
        ("type-checker.get-var", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_get_var", HSVar "gstype_checker_get_var")),
        ("type-checker.incorrect-type", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_incorrect_type", HSVar "gstype_checker_incorrect_type")),
        ("type-checker.map", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_map", HSVar "gstype_checker_map")),
        ("type-checker.mapM", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_mapM", HSVar "gstype_checker_mapM")),
        ("type-checker.new-unifiable-var", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gs_type_checker_new_unifiable_var", HSVar "gs_type_checker_new_unifiable_var")),
        ("type-checker.run", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_run", HSVar "gstype_checker_run")),
        ("type-checker.set-var", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_set_var", HSVar "gstype_checker_set_var")),
        ("type-checker.subst", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_subst", HSVar "gstype_checker_subst")),
        ("type-checker.unify", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_unify", HSVar "gstype_checker_unify")),
        ("type-checker.unit", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_unit", HSVar "gstype_checker_unit")),
        ("type-checker.with-type-env", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_with_type_env", HSVar "gstype_checker_with_type_env")),
        ("type-checker.zip-withM", (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_zipWithM", HSVar "gstype_checker_zipWithM")),
        ("type.app", (Set.singleton $ HSIVar "GSDL.AST" "gstype_app", HSVar "gstype_app")),
        ("type.const", (Set.singleton $ HSIVar "GSDL.AST" "gstype_const", HSVar "gstype_const")),
        ("type.empty-signature", (Set.singleton $ HSIVar "GSDL.AST" "gstype_empty_signature", HSVar "gstype_empty_signature")),
        ("type.env.empty",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_empty", HSVar "gstype_env_empty")),
        ("type.env.implicits.get", (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_implicits_get", HSVar "gstype_env_implicits_get")),
        ("type.env.implicits.insert",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_implicits_insert", HSVar "gstype_env_implicits_insert")),
        ("type.env.overlay",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_overlay", HSVar "gstype_env_overlay")),
        ("type.env.type.get", (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_type_get", HSVar "gstype_env_type_get")),
        ("type.env.type.insert",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_type_insert", HSVar "gstype_env_type_insert")),
        ("type.env.types",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_types", HSVar "gstype_env_types")),
        ("type.env.var-types",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_var_types", HSVar "gstype_env_var_types")),
        ("type.env.vars",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_vars", HSVar "gstype_env_vars")),
        ("type.env.view-type.get", (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_view_type_get", HSVar "gstype_env_view_type_get")),
        ("type.env.view-type.insert", (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_view_type_insert", HSVar "gstype_env_view_type_insert")),
        ("type.env.views",  (Set.singleton $ HSIVar "GSDL.AST" "gstype_env_views", HSVar "gstype_env_views")),
        ("type.forall", (Set.singleton $ HSIVar "GSDL.AST" "gstype_forall", HSVar "gstype_forall")),
        ("type.function", (Set.singleton $ HSIVar "GSDL.AST" "gstype_function", HSVar "gstype_function")),
        ("type.signature", (Set.singleton $ HSIVar "GSDL.AST" "gstype_signature", HSVar "gstype_signature")),
        ("type.unifiable-var", (Set.singleton $ HSIVar "GSDL.AST" "gstype_unifiable_var", HSVar "gstype_unifiable_var")),
        ("undefined", (Set.singleton $ HSIVar "GSI.StdLib" "gsundefined", HSVar "gsundefined")),
        ("validate-document", (Set.singleton $ HSIVar "GSDL.Validate" "gsvalidate_document", HSVar "gsvalidate_document")),
        ("validate-qloitem", (Set.singleton $ HSIVar "GSDL.Validate" "gsvalidate_qloitem", HSVar "gsvalidate_qloitem")),
        ("validator.env.empty", (Set.singleton $ HSIVar "GSDL.Validator" "gsvalidator_env_empty", HSVar "gsvalidator_env_empty")),
        ("validator.env.in-scope", (Set.singleton $ HSIVar "GSDL.Validator" "gsvalidator_env_in_scope", HSVar "gsvalidator_env_in_scope")),
        ("validator.env.in-scope.insert", (Set.singleton $ HSIVar "GSDL.Validator" "gsvalidator_env_in_scope_insert", HSVar "gsvalidator_env_in_scope_insert")),
        ("whitespace", (Set.singleton $ HSIVar "GSDL.Syntax" "whitespace", HSVar "whitespace")),
        ("zip", (Set.singleton $ HSIVar "GSI.List" "gszip", HSVar "gszip")),
        ("||", (Set.singleton $ HSIVar "GSI.Bool" "gsshort_circuit_or", HSVar "gsshort_circuit_or")),
        ("λ", (Set.singleton $ HSIVar "GSI.StdLib" "gslambda", HSVar "gslambda")),
        ("∘", (Set.singleton $ HSIVar "GSI.StdLib" "gscompose", HSVar "gscompose")),
        ("≠", (Set.singleton $ HSIVar "GSI.Rune" "gsrune_neq", HSVar "gsrune_neq"))
    ],
    gsimplicits = Map.fromList [
        ("error", [ ImHere ]),
        ("undefined", [ ImHere ])
    ],
    gsunaries = Map.fromList [
        ("*>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unary_then", HSVar "gsparser_unary_then")),
        ("+", (Set.singleton $ HSIVar "GSI.Natural" "gsnatural_unary_plus", HSVar "gsnatural_unary_plus")),
        ("<|>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unary_or", HSVar "gsparser_unary_or"))
    ],
    gsviews = Map.fromList [
        (":", (Set.singleton $ HSIVar "GSI.List" "gscons_view", HSVar "gscons_view")),
        ("ENOENT", (Set.singleton $ HSIVar "GSI.Env" "gsENOENT_view", HSVar "gsENOENT_view")),
        ("arg.explicit", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_explicit_view", HSVar "gsarg_explicit_view")),
        ("arg.expr", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_expr_view", HSVar "gsarg_expr_view")),
        ("arg.here", (Set.singleton $ HSIVar "GSDL.AST" "gsarg_here_view", HSVar "gsarg_here_view")),
        ("category.monoidal", (Set.singleton $ HSIVar "GSDL.AST" "gscategory_monoidal_view", HSVar "gscategory_monoidal_view")),
        ("document.in", (Set.singleton $ HSIVar "GSDL.AST" "gsdocument_in_view", HSVar "gsdocument_in_view")),
        ("eq", (Set.singleton $ HSIVar "GSI.OrderTotal" "gsord_eq_view", HSVar "gsord_eq_view")),
        ("expr.app", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_app_view", HSVar "gsexpr_app_view")),
        ("expr.missing-case", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_missing_case_view", HSVar "gsexpr_missing_case_view")),
        ("expr.monoidal-pat", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_monoidal_pat_view", HSVar "gsexpr_monoidal_pat_view")),
        ("expr.natural", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_natural_view", HSVar "gsexpr_natural_view")),
        ("expr.open-expr", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_open_expr_view", HSVar "gsexpr_open_expr_view")),
        ("expr.pat", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_pat_view", HSVar "gsexpr_pat_view")),
        ("expr.qlo", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_qlo_view", HSVar "gsexpr_qlo_view")),
        ("expr.var", (Set.singleton $ HSIVar "GSDL.AST" "gsexpr_var_view", HSVar "gsexpr_var_view")),
        ("false", (Set.singleton $ HSIVar "GSI.Bool" "gsfalse_view", HSVar "gsfalse_view")),
        ("file.document", (Set.singleton $ HSIVar "GSDL.AST" "gsfile_document_view", HSVar "gsfile_document_view")),
        ("gsvalue.constr", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_constr_view", HSVar "gsvalue_constr_view")),
        ("gsvalue.error", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_error_view", HSVar "gsvalue_error_view")),
        ("gsvalue.function", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_function_view", HSVar "gsvalue_function_view")),
        ("gsvalue.natural", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_natural_view", HSVar "gsvalue_natural_view")),
        ("gsvalue.rune", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_rune_view", HSVar "gsvalue_rune_view")),
        ("gsvalue.thunk", (Set.singleton $ HSIVar "GSI.GSI" "gsvalue_thunk_view", HSVar "gsvalue_thunk_view")),
        ("gt", (Set.singleton $ HSIVar "GSI.OrderTotal" "gsord_gt_view", HSVar "gsord_gt_view")),
        ("implicit.here", (Set.singleton $ HSIVar "GSDL.AST" "gsimplicit_here_view", HSVar "gsimplicit_here_view")),
        ("just", (Set.singleton $ HSIVar "GSI.Maybe" "gsjust_view", HSVar "gsjust_view")),
        ("left", (Set.singleton $ HSIVar "GSI.Either" "gsleft_view", HSVar "gsleft_view")),
        ("lt", (Set.singleton $ HSIVar "GSI.OrderTotal" "gsord_lt_view", HSVar "gsord_lt_view")),
        ("nil", (Set.singleton $ HSIVar "GSI.List" "gsnil_view", HSVar "gsnil_view")),
        ("nothing", (Set.singleton $ HSIVar "GSI.Maybe" "gsnothing_view", HSVar "gsnothing_view")),
        ("order-of-being.open-expr", (Set.singleton $ HSIVar "GSDL.AST" "gsorder_of_being_open_expr_view", HSVar "gsorder_of_being_open_expr_view")),
        ("order-of-being.pattern", (Set.singleton $ HSIVar "GSDL.AST" "gsorder_of_being_pattern_view", HSVar "gsorder_of_being_pattern_view")),
        ("ordered-map.empty", (Set.singleton $ HSIVar "GSI.Map" "gsordered_map_empty_view", HSVar "gsordered_map_empty_view")),
        ("parser.prim.symbol-or-eof", (Set.singleton $ HSIVar "GSI.Parser" "gsprim_symbol_view", HSVar "gsprim_symbol_view")),
        ("parser.prim.unit-plus", (Set.singleton $ HSIVar "GSI.Parser" "gsprim_unit_plus_view", HSVar "gsprim_unit_plus_view")),
        ("pat.app", (Set.singleton $ HSIVar "GSDL.AST" "gspat_app_view", HSVar "gspat_app_view")),
        ("pat.discard", (Set.singleton $ HSIVar "GSDL.AST" "gspat_discard_view", HSVar "gspat_discard_view")),
        ("pat.var", (Set.singleton $ HSIVar "GSDL.AST" "gspat_var_view", HSVar "gspat_var_view")),
        ("pat.view", (Set.singleton $ HSIVar "GSDL.AST" "gspat_view_view", HSVar "gspat_view_view")),
        ("qloitem.char", (Set.singleton $ HSIVar "GSDL.AST" "gsqloitem_char_view", HSVar "gsqloitem_char_view")),
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright_view", HSVar "gsright_view")),
        ("syntactic-category.lambda-like", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_lambda_like_view", HSVar "gssyntactic_category_lambda_like_view")),
        ("syntactic-category.variable", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_variable_view", HSVar "gssyntactic_category_variable_view")),
        ("syntactic-category.where", (Set.singleton $ HSIVar "GSDL.AST" "gssyntactic_category_where_view", HSVar "gssyntactic_category_where_view")),
        ("true", (Set.singleton $ HSIVar "GSI.Bool" "gstrue_view", HSVar "gstrue_view")),
        ("type.app", (Set.singleton $ HSIVar "GSDL.AST" "gstype_app_view", HSVar "gstype_app_view")),
        ("type.const", (Set.singleton $ HSIVar "GSDL.AST" "gstype_const_view", HSVar "gstype_const_view")),
        ("type.forall", (Set.singleton $ HSIVar "GSDL.AST" "gstype_forall_view", HSVar "gstype_forall_view")),
        ("type.function", (Set.singleton $ HSIVar "GSDL.AST" "gstype_function_view", HSVar "gstype_function_view")),
        ("type.signature", (Set.singleton $ HSIVar "GSDL.AST" "gstype_signature_view", HSVar "gstype_signature_view")),
        ("type.unifiable-var", (Set.singleton $ HSIVar "GSDL.AST" "gstype_unifiable_var_view", HSVar "gstype_unifiable_var_view"))
    ],
    gssignatures = Map.fromList [
        ("λ", \ as -> case as of
            (_, EPat p) : (_, EOpen b) : _ -> return [ Nothing, Just (SigOpen (boundVars p)) ]
            _ -> return []
        ),
        ("case", \ as -> case as of
            (_, EPat p) : (_, EOpen b) : _ -> return [ Nothing, Just (SigOpen (boundVars p)) ]
            _ -> return []
        ),
        ("for", \ as -> case as of
            (_, EGens gs _) : (_, EOpen b) : _ -> return [ Nothing, Just $ SigOpen $ gensBoundVars $ map (\ (_, g) -> g) gs ]
            _ -> return []
        ),
        ("impfor", \ as -> case as of
            (_, EImpGens gs _) : (_, EOpen b) : _ -> return [ Nothing, Just $ SigOpen $ gensBoundVars $ map (\ (_, g) -> g) gs ]
            _ -> return []
        ),
        ("either.for", \ as -> case as of
            (_, EMonadGens gs _) : (_, EOpen b) : _ -> return [
                Just $ SigMonad SM{
                    gsunit = (Set.singleton $ HSIVar "GSI.Either" "gsright", HSVar "gsright"),
                    gsbind = (Set.singleton $ HSIVar "GSI.Either" "gseitherbind", HSVar "gseitherbind"),
                    gsmap = (Set.singleton $ HSIVar "GSI.Either" "gseithermap", HSVar "gseithermap")
                },
                Just $ SigOpen $ gensBoundVars $ map (\ ( _, g) -> g) gs
              ]
            _ -> return []
        ),
        ("parser.for", \ as -> case as of
            (_, EMonadGens gs _) : (_, EOpen b) : _ -> return [
                Just $ SigMonad SM{
                    gsunit = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unit", HSVar "gsparser_unit"),
                    gsbind = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_bind", HSVar "gsparser_bind"),
                    gsmap = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_map", HSVar "gsparser_map")
                },
                Just $ SigOpen $ gensBoundVars $ map (\ ( _, g) -> g) gs
              ]
            _ -> return []
        ),
        ("type-checker.for", \ as -> case as of
            (_, EMonadGens gs _) : (_, EOpen b) : _ -> return [
                Just $ SigMonad SM{
                    gsunit = (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_unit", HSVar "gstype_checker_unit"),
                    gsbind = (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_bind", HSVar "gstype_checker_bind"),
                    gsmap = (Set.singleton $ HSIVar "GSDL.TypeChecker" "gstype_checker_map", HSVar "gstype_checker_map")
                },
                Just $ SigOpen $ gensBoundVars $ map (\ ( _, g) -> g) gs
              ]
            _ -> return []
        )
    ],
    gscategories = Map.fromList [
        ("case", \ as -> case as of
            (_, EPat p) : (_, EOpen b) : _ -> return [ Just Monoidal, Nothing ]
            _ -> return []
        )
    ]
  }

data Env = Env {
    gsimplicits :: Map String [Implicit],
    gssignatures :: Map String ([(Pos, Expr)] -> Compiler [Maybe Signature]),
    gscategories :: Map String ([(Pos, Expr)] -> Compiler [Maybe Category]),
    gsunaries :: Map String (Set HSImport, HSExpr),
    gsvars :: Map String (Set HSImport, HSExpr),
    gsviews :: Map String (Set HSImport, HSExpr)
  }

boundVars :: Pattern -> Set String
boundVars (PVar _ x) = Set.singleton x
boundVars (PDiscard _) = Set.empty
boundVars (PView _ _) = Set.empty
boundVars (PApp p0 p1) = boundVars p0 `Set.union` boundVars p1
boundVars p = $gsfatal $ "boundVars " ++ patCode p ++ " next"

gensBoundVars :: [Generator] -> Set String
gensBoundVars gs = Set.unions $ map genBoundVars gs

genBoundVars :: Generator -> Set String
genBoundVars (MatchGenerator x _ _) = Set.singleton x
genBoundVars (ExecGenerator _ _) = Set.empty
genBoundVars (BindGenerator x _ _) = Set.singleton x
genBoundVars g = $gsfatal $ "genBoundVars " ++ genCode g ++ " next"

data Implicit
  = ImHere

imCode :: Implicit -> String
imCode ImHere = "ImHere"

data Signature
  = SigOpen (Set String)
  | SigMonad SigMonad

data SigMonad = SM{
    gsunit :: (Set HSImport, HSExpr),
    gsbind :: (Set HSImport, HSExpr),
    gsmap :: (Set HSImport, HSExpr)
  }

data Category
  = Monoidal

sigCode :: Signature -> String
sigCode s = s `seq` $gsfatal "sigCode next"

catCode :: Category -> String
catCode c = c `seq` $gsfatal "catCode next"
