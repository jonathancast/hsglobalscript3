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

import GSI.Util (Pos(..), compilationTime, gsfatal, fmtPos)

import HSGS.Parser (Parser, parse, string, Advanceable(..), advanceStr)
import HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Arg(..), Pattern(..), PQLOItem(..), Generator(..), Param(..), interpolation, quote, scCode, eCode, qloiCode, argCode, patCode, pqloiCode, genCode)
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
      else if irf then return ()
      else do
        hPutStrLn stderr $ a ++ ": Could not load: not a file or directory"
        exitWith $ ExitFailure 1

needToRecompile a = catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)
    (do
        t0 <- getModificationTime a
        t1 <- getModificationTime (mkHSFile a)
        return $ t0 > t1 || $compilationTime > t1
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
compileArg env pos (EPat p) s (Just Fallible) = lift $ compileMonoidalPatArg env pos p
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
    w_ch pos ds (QQChar _ ch:qis) | ch `elem` "§\\{}[]" = w_ch pos (ds . (ch:)) qis
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
compileExpr env (EQLO pos0 "r" [QQChar pos1 ch]) | ch `elem` "()[]{}" = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcchar_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcchar_w" `HSApp` hspos pos1 `HSApp` HSChar ch
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
compileMonoidalPat env (PQLO pos "r" [PQChar pos1 ch]) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcrunepattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcrunepattern_w" `HSApp` hspos pos1 `HSApp` HSChar ch
  )
compileMonoidalPat env (PQLO pos "r" [qi]) = $gsfatal $ "compileMonoidalPat (PQLO pos \"r\" [" ++ pqloiCode qi ++ "]) next"
compileMonoidalPat env (PQLO pos "r" []) = $gsfatal $ "compileMonoidalPat (PQLO pos \"r\" []) next"
compileMonoidalPat env (PQLO pos "r" qs) = $gsfatal $ "compileMonoidalPat (PQLO pos \"r\" qs) next"
compileMonoidalPat env (PQLO pos "qq" s) = w s
  where
    w [] = return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcviewpattern_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.List" "gsnil_view" ],
        HSVar "gsbcviewpattern_w" `HSApp` hspos pos `HSApp` HSVar "gsnil_view" `HSApp` HSList []
      )
    w (PQChar pos1 ch:qis) = cons_pattern pos1 ch (w qis)
    w (PQQChar pos1 ch:qis) | ch `elem` "[]" = cons_pattern pos1 ch (w qis)
    w (PQQChar pos1 ch:qis) = $gsfatal $ "w (PQQChar " ++ show ch ++ " next"
    w [PQInterpPat pos1 p] = compileMonoidalPat env p
    w (qi:qis) = $gsfatal $ "w " ++ pqloiCode qi ++ " next"

    cons_pattern pos1 ch k = do
        (is, p) <- k
        return (
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcviewpattern_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.List" "gscons_view", HSIType "GSI.Value" "GSArg", HSIVar "GSI.ByteCode" "gsbcrunepattern_w" ]
                `Set.union` is
            ,
            HSVar "gsbcviewpattern_w" `HSApp` hspos pos `HSApp` HSVar "gscons_view" `HSApp` HSList [
                HSConstr "GSArgExpr" `HSApp` hspos pos1 `HSApp` (HSVar "gsbcrunepattern_w" `HSApp` hspos pos1 `HSApp` HSChar ch),
                HSConstr "GSArgExpr" `HSApp` hspos pos1 `HSApp` p
            ]
          )
compileMonoidalPat env (PQLO pos0 q s) = $gsfatal $ "compileMonoidalPat (PQLO pos " ++ show q ++ " s) next"
compileMonoidalPat env p = $gsfatal $ "compileMonoidalPat " ++ patCode p ++ " next"

compileNonMonoidalPat :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compileNonMonoidalPat env pos p = do
    (isp, hsp) <- compilePat env p
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIVar "GSI.StdLib" "gsinfalliblepattern", HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` isp,
        HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` HSVar "gsinfalliblepattern" `HSApp` HSList [ HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hsp ]
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
compilePat env (PApp p0 p1) = compilePatApp env p0 [p1]
compilePat env p = $gsfatal $ "compilePat " ++ patCode p ++ " next"

compilePatApp :: Env -> Pattern -> [Pattern] -> Either String (Set HSImport, HSExpr)
compilePatApp env (PVar pos v) _ = Left $ fmtPos pos $ "Cannot apply pattern variable '" ++ v ++ " to arguments"
compilePatApp env (PApp p0 p1) as = compilePatApp env p0 (p1 : as)
compilePatApp env p as = $gsfatal $ "compilePatApp " ++ patCode p ++ " next"

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
    gsvars = Map.fromList $ map (\ (gsn, hsm, hsn) -> (gsn, (Set.singleton $ HSIVar hsm hsn, HSVar hsn))) $ [
        ("$", "GSI.StdLib", "gsapply_fn"),
        ("&&", "GSI.Bool", "gsshort_circuit_and"),
        ("*", "GSI.Natural", "gsnatural_multiply"),
        ("*>", "GSI.Parser", "gsparser_then"),
        ("+", "GSI.Natural", "gsnatural_add"),
        ("-", "GSI.Natural", "gsnatural_subtract"),
        ("-?", "GSI.Natural", "gsnatural_subtract_maybe"),
        (":", "GSI.List", "gscons"),
        ("<*", "GSI.Parser", "gsparser_after"),
        ("<*>", "GSI.Parser", "gsparser_app"),
        ("<>", "GSI.List", "gsappend"),
        ("<|>", "GSI.Parser", "gsparser_or"),
        ("abend", "GSI.Env", "gsabend"),
        ("addition.+", "GSI.Addition", "gsaddition_plus"),
        ("advance-rune", "GSI.Parser", "gsparser_advanceRune"),
        ("advance-string", "GSI.Parser", "gsparser_advance_string"),
        ("analyze", "GSI.StdLib", "gsanalyze"),
        ("analyze-type-checker-M", "GSDL.TypeChecker", "gstype_checker_analyze"),
        ("analyzeM", "GSI.StdLib", "gsanalyzeM"),
        ("annotate-document", "GSDL.Annotate", "gsannotate_document"),
        ("annotate-expr", "GSDL.Annotate", "gsannotate_expr"),
        ("annotate-qloitem", "GSDL.Annotate", "gsannotate_qloitem"),
        ("annotator.env.category.get", "GSDL.Annotator", "gsannotator_env_category_get"),
        ("annotator.env.category.insert", "GSDL.Annotator", "gsannotator_env_category_insert"),
        ("annotator.env.empty", "GSDL.Annotator", "gsannotator_env_empty"),
        ("annotator.env.namespace.get", "GSDL.Annotator", "gsannotator_env_namespace_get"),
        ("annotator.env.namespace.insert", "GSDL.Annotator", "gsannotator_env_namespace_insert"),
        ("annotator.global-env", "GSDL.GlobalEnv", "gsannotator_global_env"),
        ("arg.explicit", "GSDL.AST", "gsarg_explicit"),
        ("arg.expr", "GSDL.AST", "gsarg_expr"),
        ("arg.here", "GSDL.AST", "gsarg_here"),
        ("case", "GSI.StdLib", "gscase"),
        ("category.fallible", "GSDL.AST", "gscategory_fallible"),
        ("char", "GSI.Parser", "gsparser_char"),
        ("compile-document", "GSDL.Compile", "gscompile_document"),
        ("compiler.env.empty", "GSDL.Compiler", "gscompiler_env_empty"),
        ("compiler.env.value.get", "GSDL.Compiler", "gscompiler_env_value_get"),
        ("compiler.env.value.insert", "GSDL.Compiler", "gscompiler_env_value_insert"),
        ("compiler.env.view.get", "GSDL.Compiler", "gscompiler_env_view_get"),
        ("compiler.env.view.insert", "GSDL.Compiler", "gscompiler_env_view_insert"),
        ("concat", "GSI.List", "gsconcat"),
        ("create-thread", "GSI.GSI", "gsicreateThread"),
        ("dest-comp.char", "GSDL.HSGS.AST", "gsdest_comp_char"),
        ("dest-comp.expr", "GSDL.HSGS.AST", "gsdest_comp_expr"),
        ("dest-comp.imports", "GSDL.HSGS.AST", "gsdest_comp_imports"),
        ("dest-comp.pos", "GSDL.HSGS.AST", "gsdest_comp_pos"),
        ("dir.read", "GSI.Env", "gsdir_read"),
        ("document.in", "GSDL.AST", "gsdocument_in"),
        ("drop", "GSI.List", "gsdrop"),
        ("either.>>=", "GSI.Either", "gseitherbind"),
        ("either.for", "GSI.Either", "gseitherFor"),
        ("either.map", "GSI.Either", "gseithermap"),
        ("either.map2", "GSI.Either", "gseithermap2"),
        ("either.mapM", "GSI.Either", "gseither_mapM"),
        ("empty", "GSI.Parser", "gsempty"),
        ("eq", "GSI.OrderTotal", "gsord_eq"),
        ("error", "GSI.StdLib", "gserror"),
        ("exec-main-thread", "GSI.GSI", "gsiexecMainThread"),
        ("expr", "GSDL.Syntax", "expr"),
        ("expr.app", "GSDL.AST", "gsexpr_app"),
        ("expr.atom", "GSDL.Syntax", "gsexpr_atom"),
        ("expr.fallible-pat", "GSDL.AST", "gsexpr_fallible_pat"),
        ("expr.missing-case", "GSDL.AST", "gsexpr_missing_case"),
        ("expr.natural", "GSDL.AST", "gsexpr_natural"),
        ("expr.open-expr", "GSDL.AST", "gsexpr_open_expr"),
        ("expr.pat", "GSDL.AST", "gsexpr_pat"),
        ("expr.qlo", "GSDL.AST", "gsexpr_qlo"),
        ("expr.structure-literal", "GSDL.AST", "gsexpr_structure_literal"),
        ("expr.var", "GSDL.AST", "gsexpr_var"),
        ("false", "GSI.Bool", "gsfalse"),
        ("file", "GSDL.Syntax", "file"),
        ("file.document", "GSDL.AST", "gsfile_document"),
        ("file.name.extension.get", "GS.FileName", "gsfile_name_extension_get"),
        ("file.name.extension.set", "GS.FileName", "gsfile_name_extension_set"),
        ("file.read", "GSI.Env", "gsfileRead"),
        ("file.stat", "GSI.Env", "gsfileStat"),
        ("file.write", "GSI.Env", "gsfile_write"),
        ("filter", "GSI.List", "gsfilter"),
        ("fmtdecimal", "GSI.String", "gsfmtdecimal"),
        ("foldl", "GSI.List", "gsfoldl"),
        ("foldr", "GSI.List", "gsfoldr"),
        ("for", "GSI.StdLib", "gsfor"),
        ("get-pos", "GSI.Parser", "gsparser_getPos"),
        ("gsae", "GSI.GSI", "gsigsae"),
        ("gsapply", "GSI.GSI", "gsigsapply"),
        ("gsav", "GSI.GSI", "gsigsav"),
        ("gsbcapply", "GSI.GSI", "gsigsbcapply"),
        ("gsbcarg", "GSI.GSI", "gsigsbcarg"),
        ("gsbcdiscardpattern", "GSI.GSI", "gsigsbcdiscardpattern"),
        ("gsbcenter", "GSI.GSI", "gsigsbcenter"),
        ("gsbcinsufficientcases", "GSI.GSI", "gsigsbcinsufficientcases"),
        ("gsbclfield", "GSI.GSI", "gsigsbclfield"),
        ("gsbcnatural", "GSI.GSI", "gsigsbcnatural"),
        ("gsbcundefined", "GSI.GSI", "gsigsbcundefined"),
        ("gsbcvarpattern", "GSI.GSI", "gsigsbcvarpattern"),
        ("gsbcviewpattern", "GSI.GSI", "gsigsbcviewpattern"),
        ("gsbcwithhere", "GSI.GSI", "gsigsbcwithhere"),
        ("gseval-sync", "GSI.GSI", "gsigsevalSync"),
        ("gsfmt-error", "GSI.GSI", "gsigsfmtError"),
        ("gsi.monad", "GSI.GSI", "gsi_monad"),
        ("gsiae", "GSI.GSI", "gsigsiae"),
        ("gsiagv", "GSI.GSI", "gsigsiagv"),
        ("gsinfalliblepattern", "GSI.StdLib", "gsinfalliblepattern"),
        ("gsinject", "GSI.GSI", "gsigsinject"),
        ("gsintbcdiscardpattern", "GSI.GSI", "gsigsintbcdiscardpattern"),
        ("gsintbceapply", "GSI.GSI", "gsigsintbceapply"),
        ("gsintbcfenter", "GSI.GSI", "gsigsintbcfenter"),
        ("gsintbcgapply", "GSI.GSI", "gsigsintbcgapply"),
        ("gsintbcgenter", "GSI.GSI", "gsigsintbcgenter"),
        ("gsintbcinsufficientcases", "GSI.GSI", "gsigsintbcinsufficientcases"),
        ("gsintbcnatural", "GSI.GSI", "gsigsintbcnatural"),
        ("gsintbcopenexpr", "GSI.GSI", "gsigsintbcopenexpr"),
        ("gsintbcundefined", "GSI.GSI", "gsigsintbcundefined"),
        ("gsintbcvarpattern", "GSI.GSI", "gsigsintbcvarpattern"),
        ("gsintbcwithhere", "GSI.GSI", "gsigsintbcwithhere"),
        ("gsintthunk", "GSI.GSI", "gsigsintthunk"),
        ("gsio.file.read", "GSI.GSIO", "gsio_file_read"),
        ("gsio.monad", "GSI.GSIO", "gsio_monad"),
        ("gsmain", "GSI.Main", "gsmain"),
        ("gsthunk", "GSI.GSI", "gsigsthunk"),
        ("gsundefined", "GSI.GSI", "gsigsundefined"),
        ("gsv", "GSI.Log", "gsloggsv"),
        ("gsvalue.constr", "GSI.GSI", "gsvalue_constr"),
        ("gsvar", "GSI.GSI", "gsigsvar"),
        ("gsvar.compare", "GSI.GSI", "gsigsvar_compare"),
        ("gsvar.fmt-atom", "GSI.GSI", "gsigsvar_fmtAtom"),
        ("gsvar.fmt-bind-atom", "GSI.GSI", "gsigsvar_fmtBindAtom"),
        ("gsvar.name", "GSI.GSI", "gsigsvar_name"),
        ("gsvar.≡", "GSI.GSI", "gsigsvar_eq"),
        ("gt", "GSI.OrderTotal", "gsord_gt"),
        ("hsexpr.app", "GSDL.HSGS.AST", "gshsexpr_app"),
        ("hsexpr.applyvar", "GSDL.HSGS.AST", "gshsexpr_applyvar"),
        ("hsexpr.fmt", "GSDL.HSGS.AST", "gshsexpr_fmt"),
        ("hsexpr.fmt-atom", "GSDL.HSGS.AST", "gshsexpr_fmt_atom"),
        ("hsexpr.int", "GSDL.HSGS.AST", "gshsexpr_int"),
        ("hsexpr.list", "GSDL.HSGS.AST", "gshsexpr_list"),
        ("hsexpr.string", "GSDL.HSGS.AST", "gshsexpr_string"),
        ("hsexpr.var", "GSDL.HSGS.AST", "gshsexpr_var"),
        ("hsgs.compiler.env.empty", "GSDL.HSGS.Compiler", "gshsgs_compiler_env_empty"),
        ("hsgs.compiler.env.var.get", "GSDL.HSGS.Compiler", "gshsgs_compiler_env_var_get"),
        ("hsgs.compiler.env.var.insert", "GSDL.HSGS.Compiler", "gshsgs_compiler_env_var_insert"),
        ("hsimport.<=>", "GSDL.HSGS.AST", "gshsimport_cmp"),
        ("hsimport.fmt", "GSDL.HSGS.AST", "gshsimport_fmt"),
        ("hsimport.type", "GSDL.HSGS.AST", "gshsimport_type"),
        ("hsimport.var", "GSDL.HSGS.AST", "gshsimport_var"),
        ("ident", "GSDL.Syntax", "ident"),
        ("ident-chars", "GSDL.Syntax", "ident_chars"),
        ("impanalyzeM", "GSI.StdLib", "gsanalyzeImpM"),
        ("impfor", "GSI.StdLib", "gsimpfor"),
        ("implicit.here", "GSDL.AST", "gsimplicit_here"),
        ("impmapM", "GS.Util", "gsimpmapM"),
        ("impunit", "GSI.StdLib", "gsimpunit"),
        ("instantiate", "GSDL.TypeChecker", "gstype_checker_instantiate"),
        ("interpolation", "GSDL.HSGS.Syntax", "interpolation"),
        ("is-ascii-digit", "GSI.Rune", "gsisAsciiDigit"),
        ("is-letter", "GSI.Rune", "gsis_letter"),
        ("is-lower", "GSI.Rune", "gsisLower"),
        ("is-space", "GSI.Rune", "gsisSpace"),
        ("is-upper", "GSI.Rune", "gsis_upper"),
        ("just", "GSI.Maybe", "gsjust"),
        ("kind.pointed", "GSDL.AST", "gskind_pointed"),
        ("left", "GSI.Either", "gsleft"),
        ("lexeme", "GSDL.Syntax", "lexeme"),
        ("list.<=>", "GSI.List", "gslist_compare"),
        ("list.∈", "GSI.List", "gslist_member"),
        ("list.∉", "GSI.List", "gslist_not_member"),
        ("list.≡", "GSI.List", "gslist_eq"),
        ("log.<>", "GSI.StdLib", "gscompose"),
        ("log.char", "GSI.Log", "gslogchar"),
        ("log.dstr", "GSI.Log", "gslog_dstr"),
        ("log.fmt", "GSI.Log", "gslog_fmt"),
        ("log.nil", "GSI.Log", "gslogempty"),
        ("log.str", "GSI.Log", "gslog_str"),
        ("lt", "GSI.OrderTotal", "gsord_lt"),
        ("many", "GSI.Parser", "gsmany"),
        ("many1", "GSI.Parser", "gsmany1"),
        ("map", "GSI.List", "gsmap"),
        ("map2", "GSI.List", "gsmap2"),
        ("map3", "GSI.List", "gsmap3"),
        ("mapM0", "GSI.Monad", "gsmonad_mapM0"),
        ("matching", "GSI.Parser", "gsparser_matching"),
        ("max-many", "GSI.Parser", "gsmax_many"),
        ("maybe", "GSI.Maybe", "gsmaybe"),
        ("monad.>>=", "GSI.Monad", "gsmonad_bind"),
        ("natural.addition", "GSI.Natural", "gsnatural_addition"),
        ("natural.div-mod", "GSI.Natural", "gsnatural_div_mod"),
        ("natural.fmt-decimal", "GSI.Format", "gsnatural_fmt_decimal"),
        ("natural.read-decimal", "GSI.Format", "gsnatural_read_decimal"),
        ("natural.≡", "GSI.Natural", "gsnatural_eq"),
        ("nil", "GSI.List", "gsnil"),
        ("not", "GSI.Bool", "gsnot"),
        ("not-followed-by", "GSI.Parser", "gsparser_notFollowedBy"),
        ("nothing", "GSI.Maybe", "gsnothing"),
        ("op-chars", "GSDL.Syntax", "op_chars"),
        ("optional", "GSI.Parser", "gsparser_optional"),
        ("ord.<|>", "GSI.OrderTotal", "gsord_or"),
        ("order-of-being.open-expr", "GSDL.AST", "gsorder_of_being_open_expr"),
        ("order-of-being.pattern", "GSDL.AST", "gsorder_of_being_pattern"),
        ("ordered-map.empty", "GSI.Map", "gsordered_map_empty"),
        ("ordered-map.get", "GSI.Map", "gsordered_map_get"),
        ("ordered-map.insert", "GSI.Map", "gsordered_map_insert"),
        ("ordered-map.keys", "GSI.Map", "gsordered_map_keys"),
        ("ordered-map.to-list", "GSI.Map", "gsordered_map_to_list"),
        ("ordered-map.values", "GSI.Map", "gsordered_map_values"),
        ("ordered-map.∪", "GSI.Map", "gsordered_map_union"),
        ("ordered-set.empty", "GSI.Set", "gsordered_set_empty"),
        ("ordered-set.from-list", "GSI.Set", "gsordered_set_from_list"),
        ("ordered-set.insert", "GSI.Set", "gsordered_set_insert"),
        ("ordered-set.singleton", "GSI.Set", "gsordered_set_singleton"),
        ("ordered-set.to-list", "GSI.Set", "gsordered_set_to_list"),
        ("ordered-set.∈", "GSI.Set", "gsordered_set_member"),
        ("ordered-set.∪", "GSI.Set", "gsordered_set_union"),
        ("parse", "GSI.Parser", "gsparse"),
        ("parse-partial", "GSI.Parser", "gsparse_partial"),
        ("parser-run", "GSI.Parser", "gsparser_run"),
        ("parser.>>=", "GSI.Parser", "gsparser_bind"),
        ("parser.choice", "GSI.Parser", "gsparser_choice"),
        ("parser.env.else.get", "GSDL.Parser", "gsdl_parser_env_else_get"),
        ("parser.env.else.insert", "GSDL.Parser", "gsdl_parser_env_else_insert"),
        ("parser.env.empty", "GSDL.Parser", "gsdl_parser_env_empty"),
        ("parser.env.missing-body.get", "GSDL.Parser", "gsdl_parser_env_missing_body_get"),
        ("parser.env.missing-body.insert", "GSDL.Parser", "gsdl_parser_env_missing_body_insert"),
        ("parser.env.missing-else.get", "GSDL.Parser", "gsdl_parser_env_missing_else_get"),
        ("parser.env.missing-else.insert", "GSDL.Parser", "gsdl_parser_env_missing_else_insert"),
        ("parser.env.qlo.get", "GSDL.Parser", "gsdl_parser_env_qlo_get"),
        ("parser.env.qlo.insert", "GSDL.Parser", "gsdl_parser_env_qlo_insert"),
        ("parser.env.signature.get", "GSDL.Parser", "gsdl_parser_env_signature_get"),
        ("parser.env.signature.insert", "GSDL.Parser", "gsdl_parser_env_signature_insert"),
        ("parser.env.syntactic-categories", "GSDL.Parser", "gsdl_parser_env_syntactic_categories"),
        ("parser.env.syntactic-category.get", "GSDL.Parser", "gsdl_parser_env_syntactic_category_get"),
        ("parser.env.syntactic-category.insert", "GSDL.Parser", "gsdl_parser_env_syntactic_category_insert"),
        ("parser.eof", "GSI.Parser", "gseof"),
        ("parser.fail", "GSI.Parser", "gsparser_fail"),
        ("parser.for", "GSI.Parser", "gsparserFor"),
        ("parser.global-env", "GSDL.GlobalEnv", "gsparser_global_env"),
        ("parser.map", "GSI.Parser", "gsparser_map"),
        ("parser.map2", "GSI.Parser", "gsparser_map2"),
        ("parser.unit", "GSI.Parser", "gsparser_unit"),
        ("pat.app", "GSDL.AST", "gspat_app"),
        ("pat.bound-vars", "GSDL.Annotator", "gspat_bound_vars"),
        ("pat.discard", "GSDL.AST", "gspat_discard"),
        ("pat.var", "GSDL.AST", "gspat_var"),
        ("pat.view", "GSDL.AST", "gspat_view"),
        ("pos.fmt", "GSI.Parser", "gsposFmt"),
        ("pos.init", "GSI.Parser", "gspos_init"),
        ("pragma.whitespace", "GSDL.Syntax", "pragmawhitespace"),
        ("pragmas", "GSDL.Syntax", "pragmas"),
        ("print", "GSI.Env", "gsprint"),
        ("print-error", "GSI.Env", "gsprintError"),
        ("print-rune", "GSI.Parser", "gsparser_print_rune"),
        ("qloitem.char", "GSDL.AST", "gsqloitem_char"),
        ("quote", "GSDL.HSGS.Syntax", "quote"),
        ("quote-param.hsvs", "GSDL.HSGS.AST", "gsquote_param_hsvs"),
        ("rational.>", "GSI.Rational", "gsrational_gt"),
        ("repeat", "GSI.List", "gsrepeat"),
        ("replicate", "GSI.List", "gsreplicate"),
        ("right", "GSI.Either", "gsright"),
        ("rune.<=>", "GSI.Rune", "gsrune_compare"),
        ("rune.code-point", "GSI.Rune", "gsrune_code_point"),
        ("rune.from-code-point", "GSI.Rune", "gsrune_from_code_point"),
        ("rune.≠", "GSI.Rune", "gsrune_neq"),
        ("rune.≡", "GSI.Rune", "gsruneEq"),
        ("source-comp.char", "GSDL.HSGS.AST", "gssource_comp_char"),
        ("source-comp.declare", "GSDL.HSGS.AST", "gssource_comp_declare"),
        ("source-comp.expr", "GSDL.HSGS.AST", "gssource_comp_expr"),
        ("source-comp.imports", "GSDL.HSGS.AST", "gssource_comp_imports"),
        ("source-comp.pos", "GSDL.HSGS.AST", "gssource_comp_pos"),
        ("source-comp.value", "GSDL.HSGS.AST", "gssource_comp_value"),
        ("st.get-var", "GSI.ST", "gsstgetvar"),
        ("st.ref.new", "GSI.ST", "gsstrefnew"),
        ("st.ref.≡", "GSI.ST", "gsstrefeq"),
        ("st.run", "GSI.ST", "gsstrun"),
        ("st.set-var", "GSI.ST", "gsstsetvar"),
        ("string", "GSI.Parser", "gsparser_string"),
        ("symbol", "GSI.Parser", "gssymbol"),
        ("syntactic-category.lambda-like", "GSDL.AST", "gssyntactic_category_lambda_like"),
        ("syntactic-category.variable", "GSDL.AST", "gssyntactic_category_variable"),
        ("syntactic-category.where", "GSDL.AST", "gssyntactic_category_where"),
        ("true", "GSI.Bool", "gstrue"),
        ("type-check-arg", "GSDL.TypeCheck", "gstype_check_arg"),
        ("type-check-document", "GSDL.TypeCheck", "gstype_check_document"),
        ("type-check-expr", "GSDL.TypeCheck", "gstype_check_expr"),
        ("type-check-implicit", "GSDL.TypeCheck", "gstype_check_implicit"),
        ("type-checker.>>=", "GSDL.TypeChecker", "gstype_checker_bind"),
        ("type-checker.env.get-implicit", "GSDL.TypeChecker", "gstype_checker_env_get_implicit"),
        ("type-checker.env.get-type", "GSDL.TypeChecker", "gstype_checker_env_get_type"),
        ("type-checker.env.get-view-type", "GSDL.TypeChecker", "gstype_checker_env_get_view_type"),
        ("type-checker.fmt-type", "GSDL.TypeChecker", "gstype_checker_fmt_type"),
        ("type-checker.foldM", "GSDL.TypeChecker", "gstype_checker_foldM"),
        ("type-checker.for", "GSDL.TypeChecker", "gstype_checker_for"),
        ("type-checker.get-var", "GSDL.TypeChecker", "gstype_checker_get_var"),
        ("type-checker.incorrect-type", "GSDL.TypeChecker", "gstype_checker_incorrect_type"),
        ("type-checker.map", "GSDL.TypeChecker", "gstype_checker_map"),
        ("type-checker.mapM", "GSDL.TypeChecker", "gstype_checker_mapM"),
        ("type-checker.new-unifiable-var", "GSDL.TypeChecker", "gs_type_checker_new_unifiable_var"),
        ("type-checker.run", "GSDL.TypeChecker", "gstype_checker_run"),
        ("type-checker.set-var", "GSDL.TypeChecker", "gstype_checker_set_var"),
        ("type-checker.subst", "GSDL.TypeChecker", "gstype_checker_subst"),
        ("type-checker.unify", "GSDL.TypeChecker", "gstype_checker_unify"),
        ("type-checker.unit", "GSDL.TypeChecker", "gstype_checker_unit"),
        ("type-checker.with-type-env", "GSDL.TypeChecker", "gstype_checker_with_type_env"),
        ("type-checker.zip-withM", "GSDL.TypeChecker", "gstype_checker_zipWithM"),
        ("type.app", "GSDL.AST", "gstype_app"),
        ("type.apply", "GSDL.AST", "gstype_apply"),
        ("type.const", "GSDL.AST", "gstype_const"),
        ("type.consts", "GSDL.AST", "gstype_consts"),
        ("type.empty-signature", "GSDL.AST", "gstype_empty_signature"),
        ("type.env.empty",  "GSDL.AST", "gstype_env_empty"),
        ("type.env.implicits.get", "GSDL.AST", "gstype_env_implicits_get"),
        ("type.env.implicits.insert",  "GSDL.AST", "gstype_env_implicits_insert"),
        ("type.env.overlay",  "GSDL.AST", "gstype_env_overlay"),
        ("type.env.type.get", "GSDL.AST", "gstype_env_type_get"),
        ("type.env.type.insert",  "GSDL.AST", "gstype_env_type_insert"),
        ("type.env.types",  "GSDL.AST", "gstype_env_types"),
        ("type.env.var-types",  "GSDL.AST", "gstype_env_var_types"),
        ("type.env.vars",  "GSDL.AST", "gstype_env_vars"),
        ("type.env.view-type.get", "GSDL.AST", "gstype_env_view_type_get"),
        ("type.env.view-type.insert", "GSDL.AST", "gstype_env_view_type_insert"),
        ("type.env.views",  "GSDL.AST", "gstype_env_views"),
        ("type.forall", "GSDL.AST", "gstype_forall"),
        ("type.function", "GSDL.AST", "gstype_function"),
        ("type.signature", "GSDL.AST", "gstype_signature"),
        ("type.unifiable-var", "GSDL.AST", "gstype_unifiable_var"),
        ("undefined", "GSI.StdLib", "gsundefined"),
        ("validate-document", "GSDL.Validate", "gsvalidate_document"),
        ("validate-qloitem", "GSDL.Validate", "gsvalidate_qloitem"),
        ("validator.env.empty", "GSDL.Validator", "gsvalidator_env_empty"),
        ("validator.env.in-scope", "GSDL.Validator", "gsvalidator_env_in_scope"),
        ("validator.env.in-scope.insert", "GSDL.Validator", "gsvalidator_env_in_scope_insert"),
        ("whitespace", "GSDL.Syntax", "whitespace"),
        ("zip", "GSI.List", "gszip"),
        ("||", "GSI.Bool", "gsshort_circuit_or"),
        ("λ", "GSI.StdLib", "gslambda"),
        ("∘", "GSI.StdLib", "gscompose"),
        ("≠", "GSI.Rune", "gsrune_neq")
    ],
    gsimplicits = Map.fromList [
        ("error", [ ImHere ]),
        ("undefined", [ ImHere ]),
        ("λ", [ ImHere ])
    ],
    gsunaries = Map.fromList $ map (\ (gsn, hsm, hsn) -> (gsn, (Set.singleton $ HSIVar hsm hsn, HSVar hsn))) $ [
        ("*>", "GSI.Parser", "gsparser_unary_then"),
        ("+", "GSI.Natural", "gsnatural_unary_plus"),
        ("<|>", "GSI.Parser", "gsparser_unary_or")
    ],
    gsviews = Map.fromList $ map (\ (gsn, hsm, hsn) -> (gsn, (Set.singleton $ HSIVar hsm hsn, HSVar hsn))) $ [
        (":", "GSI.List", "gscons_view"),
        ("ENOENT", "GSI.Env", "gsENOENT_view"),
        ("arg.explicit", "GSDL.AST", "gsarg_explicit_view"),
        ("arg.expr", "GSDL.AST", "gsarg_expr_view"),
        ("arg.here", "GSDL.AST", "gsarg_here_view"),
        ("category.fallible", "GSDL.AST", "gscategory_fallible_view"),
        ("dest-comp.char", "GSDL.HSGS.AST", "gsdest_comp_char_view"),
        ("dest-comp.expr", "GSDL.HSGS.AST", "gsdest_comp_expr_view"),
        ("dest-comp.imports", "GSDL.HSGS.AST", "gsdest_comp_imports_view"),
        ("dest-comp.pos", "GSDL.HSGS.AST", "gsdest_comp_pos_view"),
        ("document.in", "GSDL.AST", "gsdocument_in_view"),
        ("eq", "GSI.OrderTotal", "gsord_eq_view"),
        ("expr.app", "GSDL.AST", "gsexpr_app_view"),
        ("expr.missing-case", "GSDL.AST", "gsexpr_missing_case_view"),
        ("expr.fallible-pat", "GSDL.AST", "gsexpr_fallible_pat_view"),
        ("expr.natural", "GSDL.AST", "gsexpr_natural_view"),
        ("expr.open-expr", "GSDL.AST", "gsexpr_open_expr_view"),
        ("expr.pat", "GSDL.AST", "gsexpr_pat_view"),
        ("expr.qlo", "GSDL.AST", "gsexpr_qlo_view"),
        ("expr.var", "GSDL.AST", "gsexpr_var_view"),
        ("false", "GSI.Bool", "gsfalse_view"),
        ("file.document", "GSDL.AST", "gsfile_document_view"),
        ("gsvalue.constr", "GSI.GSI", "gsvalue_constr_view"),
        ("gsvalue.error", "GSI.GSI", "gsvalue_error_view"),
        ("gsvalue.function", "GSI.GSI", "gsvalue_function_view"),
        ("gsvalue.implementation-failure", "GSI.GSI", "gsvalue_implementation_failure_view"),
        ("gsvalue.natural", "GSI.GSI", "gsvalue_natural_view"),
        ("gsvalue.rune", "GSI.GSI", "gsvalue_rune_view"),
        ("gsvalue.thunk", "GSI.GSI", "gsvalue_thunk_view"),
        ("gt", "GSI.OrderTotal", "gsord_gt_view"),
        ("hsexpr.app", "GSDL.HSGS.AST", "gshsexpr_app_view"),
        ("hsexpr.int", "GSDL.HSGS.AST", "gshsexpr_int_view"),
        ("hsexpr.list", "GSDL.HSGS.AST", "gshsexpr_list_view"),
        ("hsexpr.string", "GSDL.HSGS.AST", "gshsexpr_string_view"),
        ("hsexpr.var", "GSDL.HSGS.AST", "gshsexpr_var_view"),
        ("hsimport.type", "GSDL.HSGS.AST", "gshsimport_type_view"),
        ("hsimport.var", "GSDL.HSGS.AST", "gshsimport_var_view"),
        ("implicit.here", "GSDL.AST", "gsimplicit_here_view"),
        ("just", "GSI.Maybe", "gsjust_view"),
        ("left", "GSI.Either", "gsleft_view"),
        ("lt", "GSI.OrderTotal", "gsord_lt_view"),
        ("nil", "GSI.List", "gsnil_view"),
        ("nothing", "GSI.Maybe", "gsnothing_view"),
        ("order-of-being.open-expr", "GSDL.AST", "gsorder_of_being_open_expr_view"),
        ("order-of-being.pattern", "GSDL.AST", "gsorder_of_being_pattern_view"),
        ("ordered-map.empty", "GSI.Map", "gsordered_map_empty_view"),
        ("parser.prim.symbol-or-eof", "GSI.Parser", "gsprim_symbol_view"),
        ("parser.prim.unit-plus", "GSI.Parser", "gsprim_unit_plus_view"),
        ("pat.app", "GSDL.AST", "gspat_app_view"),
        ("pat.discard", "GSDL.AST", "gspat_discard_view"),
        ("pat.var", "GSDL.AST", "gspat_var_view"),
        ("pat.view", "GSDL.AST", "gspat_view_view"),
        ("qloitem.char", "GSDL.AST", "gsqloitem_char_view"),
        ("quote-param.hsvs", "GSDL.HSGS.AST", "gsquote_param_hsvs_view"),
        ("right", "GSI.Either", "gsright_view"),
        ("source-comp.char", "GSDL.HSGS.AST", "gssource_comp_char_view"),
        ("source-comp.declare", "GSDL.HSGS.AST", "gssource_comp_declare_view"),
        ("source-comp.expr", "GSDL.HSGS.AST", "gssource_comp_expr_view"),
        ("source-comp.imports", "GSDL.HSGS.AST", "gssource_comp_imports_view"),
        ("source-comp.pos", "GSDL.HSGS.AST", "gssource_comp_pos_view"),
        ("source-comp.value", "GSDL.HSGS.AST", "gssource_comp_value_view"),
        ("syntactic-category.lambda-like", "GSDL.AST", "gssyntactic_category_lambda_like_view"),
        ("syntactic-category.variable", "GSDL.AST", "gssyntactic_category_variable_view"),
        ("syntactic-category.where", "GSDL.AST", "gssyntactic_category_where_view"),
        ("true", "GSI.Bool", "gstrue_view"),
        ("type.app", "GSDL.AST", "gstype_app_view"),
        ("type.const", "GSDL.AST", "gstype_const_view"),
        ("type.forall", "GSDL.AST", "gstype_forall_view"),
        ("type.function", "GSDL.AST", "gstype_function_view"),
        ("type.signature", "GSDL.AST", "gstype_signature_view"),
        ("type.unifiable-var", "GSDL.AST", "gstype_unifiable_var_view")
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
            (_, EPat p) : (_, EOpen b) : _ -> return [ Just Fallible, Nothing ]
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
boundVars (PQLO _ "r" qis) = Set.empty
boundVars (PQLO _ "qq" qis) = Set.unions $ map w qis where
    w (PQChar _ _) = Set.empty
    w (PQQChar _ _) = Set.empty
    w (PQInterpPat _ p) = boundVars p
    w qi = $gsfatal $ "w " ++ pqloiCode qi ++ " next"
boundVars (PQLO _ q _) = $gsfatal $ "boundVars (PQLO _ " ++ q ++ " _) next"
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
  = Fallible

sigCode :: Signature -> String
sigCode s = s `seq` $gsfatal "sigCode next"

catCode :: Category -> String
catCode c = c `seq` $gsfatal "catCode next"
