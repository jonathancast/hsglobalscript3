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

import Data.Encoding.UTF8 (UTF8(..))
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Encoding (readFile, writeFile)

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
        s <- readFile a
        case compileHSGSSource m a s of
            Left err -> do
                hPutStrLn stderr err
                exitWith $ ExitFailure 1
            Right s' -> do
                writeFile (mkHSFile a) s'
      else do
        return ()

modCat "" a = mkMod a
modCat m a = m ++ '.' : mkMod a

mkMod "" = ""
mkMod ".hsgs" = ""
mkMod (c:s) = c : mkMod s

compileHSGSSource :: String -> FilePath -> String -> Either String String
compileHSGSSource m fn s =
   splitInput (Pos fn 1 1) s >>=
   compileSource m >>=
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
formatExprAtom e = $gsfatal $ "formatExprAtom " ++ hsCode e ++ " next"

compileSource :: String -> [SourceComp] -> Either String [DestComp]
compileSource m (SCChar c:scs) = (DCChar c:) <$> compileSource m scs
compileSource m (SCPos pos:scs) = (DCPos pos:) <$> compileSource m scs
compileSource m (SCImports:scs) = do
    dcs <- compileSource m scs
    return $ DCImports (gatherImports m Set.empty dcs) : dcs
compileSource m (SCArg pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileArg (processHSVS ps globalEnv) pos e Nothing) <*> compileSource m scs
compileSource m (SCExpr ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileExpr (processHSVS ps globalEnv) e) <*> compileSource m scs
compileSource m (SCOpenExpr pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileOpenExpr (processHSVS ps globalEnv) pos (processFVS ps) e) <*> compileSource m scs
compileSource m (SCOpenArg pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileOpenArg (processHSVS ps globalEnv) pos (processFVS ps) e) <*> compileSource m scs
compileSource m (SCPat ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePat globalEnv p <*> compileSource m scs
compileSource m (SCPatArg pos ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePatArg globalEnv pos p <*> compileSource m scs
compileSource m (SCBody pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBody (processHSVS ps globalEnv) pos e) <*> compileSource m scs
compileSource m (SCBind pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBind (processHSVS ps globalEnv) pos e) <*> compileSource m scs
compileSource m (SCValue pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileValue (processHSVS ps globalEnv) pos e) <*> compileSource m scs
compileSource m (sc:scs) = $gsfatal $ "compileSource " ++ scCode sc ++ " next"
compileSource m [] = return []

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
compileValue env _ (EVar pos1 v) = do
    (isv, ev) <- case Map.lookup v (gsvars env) of
        Nothing -> compileError pos1 $ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    return (isv, ev)
compileValue env pos e@EApp{} = compileThunk env pos e
compileValue env pos e = $gsfatal $ "compileValue " ++ eCode e ++ " next"

compileThunk :: Env -> Pos -> Expr -> Compiler (Set HSImport, HSExpr)
compileThunk env pos e = do
    (is, hse) <- compileExpr env e
    return (
        Set.fromList [ HSIVar "System.IO.Unsafe" "unsafePerformIO", HSIVar "GSI.Value" "gsthunk_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "unsafePerformIO" `HSApp` (HSVar "gsthunk_w" `HSApp` hspos pos `HSApp` hse)
      )

compileArg :: Env -> Pos -> Expr -> Maybe Signature -> Compiler (Set HSImport, HSExpr)
compileArg env pos e@EMissingCase{} s = compileExprToArg env pos e
compileArg env pos e@EQLO{} s = compileExprToArg env pos e
compileArg env pos (EVar pos1 v) s = case Map.lookup v (gsimplicits env) of
    Nothing -> do
        (isv, ev) <- case Map.lookup v (gsvars env) of
            Nothing -> lift $ Left $ fmtPos pos $ v ++ " not in scope"
            Just (isv, ev) -> return (isv, ev)
        return (
            Set.singleton (HSIType "GSI.Value" "GSArg") `Set.union` isv,
            HSConstr "GSArgVar" `HSApp` ev
          )
    Just _ -> compileExprToArg env pos (EVar pos1 v)
compileArg env pos (ENumber _ n) s = return (
    Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Value" "GSValue" ],
    HSConstr "GSArgVar" `HSApp` (HSConstr "GSNatural" `HSApp` HSInteger n)
  )
compileArg env pos (EPat p) s = lift $ compilePatArg env pos p
compileArg env pos (EOpen e) s = compileOpenArg env pos fvs e where
    fvs = case s of
        Nothing -> Set.empty
        Just (SigOpen vs) -> vs
        Just sg -> $gsfatal $ "compileArg (EOpen e) " ++ sigCode sg ++ " next"
compileArg env pos (EImpGens gs pos1) s = compileImpGensArg env pos gs pos1
compileArg env pos (EMonadGens gs pos1) (Just (SigMonad s)) = compileMonadGensArg env pos gs pos1 s
compileArg env pos (EMonadGens gs pos1) (Just s) = compileError pos $ "monadic generators with invalid signature " ++ sigCode s ++ "!"
compileArg env pos (EMonadGens gs pos1) Nothing = compileError pos "monadic generators with no signature!"
compileArg env pos e@EApp{} s = compileExprToArg env pos e
compileArg env pos e s = $gsfatal $ "compileArg " ++ eCode e ++ " next"

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
        (ise, hse) <- compileArg env pos1 e Nothing
        (ist, hst) <- w qis
        return (ise `Set.union` ist, hse : hst)
    w (qi:qis) = $gsfatal $ "w " ++ qloiCode qi ++ " next"

    w_ch pos ds [] = return (string_imports, [ string_expr pos (ds "") ])
    w_ch pos ds (QChar _ ch:qis) = w_ch pos (ds . (ch:)) qis
    w_ch pos ds (QQChar _ 'n':qis) = w_ch pos (ds . ('\n':)) qis
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
        (ise, hse) <- compileArg env pos1 e Nothing
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
compileExpr env (EQLO pos0 "r" [QQChar pos1 ch]) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" [QQChar pos1 " ++ show ch ++ "]) next"
compileExpr env (EQLO pos0 "r" [qi]) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" [" ++ qloiCode qi ++ "]) next"
compileExpr env (EQLO pos0 "r" (qi:qis)) = $gsfatal $ "compileExpr env (EQLO pos0 \"r\" (qi:qis)) next"
compileExpr env (EQLO pos0 q s) = $gsfatal $ "compileExpr (EQLO pos " ++ show q ++ " s) next"
compileExpr env (EApp f (ArgExpr pos1 e)) = compileApp env f [(pos1, e)]
compileExpr env (EApp f (ArgField pos1 m)) = do
    (isf, hsef) <- compileArg env pos1 f Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcfield_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` isf,
        HSVar "gsbcfield_w" `HSApp` hspos pos1 `HSApp` hsef `HSApp` (HSVar "gsvar" `HSApp` HSString m)
      )
compileExpr env (EApp f a) = $gsfatal $ "compileExpr (EApp f " ++ argCode a ++ ") next"
compileExpr env (EGens gs pos1) = compileGens env gs pos1
compileExpr env e = $gsfatal $ "compileExpr " ++ eCode e ++ " next"

compileBody :: Env -> Pos -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileBody env pos e = do
    (is, hse) <- compileArg env pos e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpbody_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpbody_w" `HSApp` hspos pos `HSApp` hse
      )

compileBind :: Env -> Pos -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileBind env pos e = do
    (is, hse) <- compileArg env pos e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpbind_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpbind_w" `HSApp` hspos pos `HSApp` hse
      )

compileOpenExpr :: Env -> Pos -> Set String -> Expr -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileOpenExpr env pos fvs e = do
    vshsvs <- forM (Set.toList fvs) $ \ v -> do
        hsv <- getGenSym
        return (v, hsv)
    let env' = env{ gsvars = Map.fromList [ (v, (Set.empty, HSVar hsv)) | (v, hsv) <- vshsvs ] `Map.union` gsvars env }
    (is, hse) <- compileExpr env' e
    let (is', hse') = foldr (\ (v, hsv) (is0, hse0) -> (
                Set.fromList [ HSIVar "GSI.ByteCode" "gsbclfield_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is0,
                HSVar "gsbclfield_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString v) `HSApp` HSVar "env" `HSApp` (HSLambda [hsv] hse0)
            ))
            (is, hse)
            vshsvs
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcarg_w", HSIType "GSI.Util" "Pos" ] `Set.union` is',
        HSVar "gsbcarg_w" `HSApp` hspos pos `HSApp` HSLambda ["env"] hse'
      )

compileOpenArg :: Env -> Pos -> Set String -> Expr -> Compiler (Set HSImport, HSExpr)
compileOpenArg env pos fvs e = do
    (is, hse) <- compileOpenExpr env pos fvs e
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileApp :: Env -> Expr -> [(Pos, Expr)] -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileApp env (EVar pos f) as = do
    (isf, ef) <- case Map.lookup f (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    as0 <- case Map.lookup f (gsimplicits env) of
        Nothing -> return []
        Just ims -> forM ims $ \ im -> case im of
            ImHere -> return (
                Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos", HSIVar "GSI.ByteCode" "gsbchere_w", HSIType "GSI.Util" "Pos" ],
                HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` (HSVar "gsbchere_w" `HSApp` hspos pos)
              )
            _ -> $gsfatal $ "Compile implicit " ++ imCode im ++ " next"
    sig <- case Map.lookup f (gssignatures env) of
        Nothing -> return []
        Just sigM -> sigM as
    as' <- mapM (\ ((pos1, e), s) -> compileArg env pos1 e s) (zip as (sig ++ repeat Nothing))
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as0 ++
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as0 ++ map (\ (_, a) -> a) as')
      )
compileApp env (EUnary pos f) as = do
    (isf, ef) <- case Map.lookup f (gsunaries env) of
        Nothing -> lift $ Left $ fmtPos pos $ "unary " ++ f ++ " not in scope"
        Just (isf, ef) -> return (isf, ef)
    as' <- mapM (\ (pos1, e) -> compileArg env pos1 e Nothing) as
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            isf :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` ef `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp env (EApp f (ArgExpr pos a)) as = compileApp env f ((pos, a):as)
compileApp env (EApp f a) as = $gsfatal $ "compileApp (EApp f " ++ argCode a ++ ") next"
compileApp env f as = $gsfatal $ "compileApp " ++ eCode f ++ " next"

compilePat :: Env -> Pattern -> Either String (Set HSImport, HSExpr)
compilePat env (PVar pos v) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarpattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcvarpattern_w" `HSApp` hspos pos `HSApp` HSString v
  )
compilePat env (PDiscard pos) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcdiscardpattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcdiscardpattern_w" `HSApp` hspos pos
  )
compilePat env (PApp p0 p1) = compilePatApp env p0 [p1]
compilePat env p@PView{} = compilePatApp env p []
compilePat env p = $gsfatal $ "compilePat " ++ patCode p ++ " next"

compilePatArg :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compilePatArg env pos p = do
    (is, e) <- compilePat env p
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` e
      )

compilePatApp :: Env -> Pattern -> [Pattern] -> Either String (Set HSImport, HSExpr)
compilePatApp env (PView pos v) as = do
    as' <- mapM (compilePat env) as
    (isv, ev) <- case Map.lookup v (gsviews env) of
        Nothing -> Left $ fmtPos pos $ "view " ++ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcviewpattern_w", HSIType "GSI.Util" "Pos" ] `Set.union` isv `Set.union` Set.unions (map (\ (is, _) -> is) as'),
        foldl HSApp (HSVar "gsbcviewpattern_w" `HSApp` hspos pos `HSApp` ev) (map (\ (_, e) -> e) as')
      )
compilePatApp env (PApp pf px) as = compilePatApp env pf (px:as)
compilePatApp env p as = $gsfatal $ "compilePatApp " ++ patCode p ++ " next"

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
    (ist, hst) <- compileMonadGens env gs pos1 s
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposemonadgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` bindis `Set.union` unitis `Set.union` is `Set.union` ist,
        HSVar "gsbccomposemonadgen_w" `HSApp` hspos pos `HSApp` (HSConstr "GSArgVar" `HSApp` bindhse) `HSApp` (HSConstr "GSArgVar" `HSApp` unithse) `HSApp` hse `HSApp` (HSLambda ["env"] hst)
      )
compileMonadGens env [] pos1 s = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptymonadgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` unitis,
    HSVar "gsbcemptymonadgen_w" `HSApp` hspos pos1 `HSApp` (HSConstr "GSArgVar" `HSApp` unithse)
  ) where
    (unitis, unithse) = gsunit s

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
    (is, hse) <- compileArg env pos1 e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcexecgen_w", HSIType "GSI.Util" "Pos", HSIType "GSI.Value" "GSArg" ] `Set.union` mapis `Set.union` is,
        HSVar "gsbcexecgen_w" `HSApp` hspos pos `HSApp` (HSConstr "GSArgVar" `HSApp` maphse) `HSApp` hse
      )
compileMonadGen env pos (BindGenerator x pos1 e) s = do
    let (mapis, maphse) = gsmap s
    (is, hse) <- compileArg env pos1 e Nothing
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
compileGenArg env pos g = $gsfatal "compileGenArg env pos g next"

compileGen :: Env -> Generator -> Compiler (Set HSImport, HSExpr)
compileGen env g = $gsfatal $ "compileGen env " ++ genCode g ++ " next"

compileImpGens :: Env -> [(Pos, Generator)] -> Pos -> Compiler (Set HSImport, HSExpr)
compileImpGens env ((pos, g):gs) pos1 = do
    (is, hse) <- compileImpGenArg env pos g
    (ist, hst) <- compileImpGens env gs pos1
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposeimpgen_w", HSIType "GSI.Util" "Pos" ] `Set.union` is `Set.union` ist,
        HSVar "gsbccomposeimpgen_w" `HSApp` hspos pos `HSApp` hse `HSApp` (HSLambda ["env"] hst)
      )
compileImpGens env [] pos1 = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptyimpgen_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcemptyimpgen_w" `HSApp` hspos pos1
  )

compileImpGenArg :: Env -> Pos -> Generator -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileImpGenArg env pos g = do
    (is, hse) <- compileImpGen env pos g
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileImpGen :: Env -> Pos -> Generator -> Compiler (Set HSImport, HSExpr)
compileImpGen env pos (ExecGenerator pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpexecbind_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpexecbind_w" `HSApp` hspos pos1 `HSApp` hse
      )
compileImpGen env pos (BindGenerator x pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing
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
        ("advance-rune", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_advanceRune", HSVar "gsparser_advanceRune")),
        ("analyze", (Set.singleton $ HSIVar "GSI.StdLib" "gsanalyze", HSVar "gsanalyze")),
        ("analyze-impM", (Set.singleton $ HSIVar "GSI.StdLib" "gsanalyzeImpM", HSVar "gsanalyzeImpM")),
        ("case", (Set.singleton $ HSIVar "GSI.StdLib" "gscase", HSVar "gscase")),
        ("char", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_char", HSVar "gsparser_char")),
        ("compile-document", (Set.singleton $ HSIVar "GSI.Main" "gscompileDocument", HSVar "gscompileDocument")),
        ("createThread", (Set.singleton $ HSIVar "GSI.GSI" "gsicreateThread", HSVar "gsicreateThread")),
        ("display-rune", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_displayRune", HSVar "gsparser_displayRune")),
        ("either-for", (Set.singleton $ HSIVar "GSI.Either" "gseitherFor", HSVar "gseitherFor")),
        ("empty", (Set.singleton $ HSIVar "GSI.Parser" "gsempty", HSVar "gsempty")),
        ("env.get-args", (Set.singleton $ HSIVar "GSI.Env" "gsenvGetArgs", HSVar "gsenvGetArgs")),
        ("error", (Set.singleton $ HSIVar "GSI.StdLib" "gserror", HSVar "gserror")),
        ("execMainThread", (Set.singleton $ HSIVar "GSI.GSI" "gsiexecMainThread", HSVar "gsiexecMainThread")),
        ("file.document", (Set.singleton $ HSIVar "GSI.Main" "gsfileDocument", HSVar "gsfileDocument")),
        ("file.read", (Set.singleton $ HSIVar "GSI.Env" "gsfileRead", HSVar "gsfileRead")),
        ("file.stat", (Set.singleton $ HSIVar "GSI.Env" "gsfileStat", HSVar "gsfileStat")),
        ("fmtdecimal", (Set.singleton $ HSIVar "GSI.String" "gsfmtdecimal", HSVar "gsfmtdecimal")),
        ("gsapply", (Set.singleton $ HSIVar "GSI.GSI" "gsigsapply", HSVar "gsigsapply")),
        ("gsiThreadData", (Set.singleton $ HSIVar "GSI.GSI" "gsigsiThreadData", HSVar "gsigsiThreadData")),
        ("gsinject", (Set.singleton $ HSIVar "GSI.GSI" "gsigsinject", HSVar "gsigsinject")),
        ("gsmain", (Set.singleton $ HSIVar "GSI.Main" "gsmain", HSVar "gsmain")),
        ("gsundefined", (Set.singleton $ HSIVar "GSI.GSI" "gsigsundefined", HSVar "gsigsundefined")),
        ("gsv", (Set.singleton $ HSIVar "GSI.Log" "gsloggsv", HSVar "gsloggsv")),
        ("impfor", (Set.singleton $ HSIVar "GSI.StdLib" "gsimpfor", HSVar "gsimpfor")),
        ("left", (Set.singleton $ HSIVar "GSI.Either" "gsleft", HSVar "gsleft")),
        ("many", (Set.singleton $ HSIVar "GSI.Parser" "gsmany", HSVar "gsmany")),
        ("matching", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_matching", HSVar "gsparser_matching")),
        ("nil", (Set.singleton $ HSIVar "GSI.List" "gsnil", HSVar "gsnil")),
        ("parse", (Set.singleton $ HSIVar "GSI.Parser" "gsparse", HSVar "gsparse")),
        ("parser-for", (Set.singleton $ HSIVar "GSI.Parser" "gsparserFor", HSVar "gsparserFor")),
        ("parser.unit", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unit", HSVar "gsparser_unit")),
        ("pos.fmt", (Set.singleton $ HSIVar "GSI.Parser" "gsposFmt", HSVar "gsposFmt")),
        ("print-error", (Set.singleton $ HSIVar "GSI.Env" "gsprintError", HSVar "gsprintError")),
        ("process-document", (Set.singleton $ HSIVar "GSI.Main" "gsprocessDocument", HSVar "gsprocessDocument")),
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright", HSVar "gsright")),
        ("rune.≡", (Set.singleton $ HSIVar "GSI.Rune" "gsruneEq", HSVar "gsruneEq")),
        ("type-check-document", (Set.singleton $ HSIVar "GSI.Main" "gstypeCheckDocument", HSVar "gstypeCheckDocument")),
        ("undefined", (Set.singleton $ HSIVar "GSI.StdLib" "gsundefined", HSVar "gsundefined"))
    ],
    gsimplicits = Map.fromList [
        ("error", [ ImHere ]),
        ("undefined", [ ImHere ])
    ],
    gsunaries = Map.fromList [
        ("<|>", (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unary_or", HSVar "gsparser_unary_or"))
    ],
    gsviews = Map.fromList [
        (":", (Set.singleton $ HSIVar "GSI.List" "gscons_view", HSVar "gscons_view")),
        ("ENOENT", (Set.singleton $ HSIVar "GSI.Env" "gsENOENT_view", HSVar "gsENOENT_view")),
        ("empty", (Set.singleton $ HSIVar "GSI.Parser" "gsempty_view", HSVar "gsempty_view")),
        ("false", (Set.singleton $ HSIVar "GSI.Bool" "gsfalse_view", HSVar "gsfalse_view")),
        ("file.document", (Set.singleton $ HSIVar "GSI.Main" "gsfileDocument_view", HSVar "gsfileDocument_view")),
        ("left", (Set.singleton $ HSIVar "GSI.Either" "gsleft_view", HSVar "gsleft_view")),
        ("nil", (Set.singleton $ HSIVar "GSI.List" "gsnil_view", HSVar "gsnil_view")),
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright_view", HSVar "gsright_view")),
        ("symbol-or-eof", (Set.singleton $ HSIVar "GSI.Parser" "gssymbol_view", HSVar "gssymbol_view")),
        ("true", (Set.singleton $ HSIVar "GSI.Bool" "gstrue_view", HSVar "gstrue_view")),
        ("unit-plus", (Set.singleton $ HSIVar "GSI.Parser" "gsunitplus_view", HSVar "gsunitplus_view"))
    ],
    gssignatures = Map.fromList [
        ("case", \ as -> case as of
            (_, EPat p) : (_, EOpen b) : _ -> return [ Nothing, Just (SigOpen (boundVars p)) ]
            _ -> return []
        ),
        ("impfor", \ as -> case as of
            (_, EImpGens gs _) : (_, EOpen b) : _ -> return [ Nothing, Just $ SigOpen $ gensBoundVars $ map (\ (_, g) -> g) gs ]
            _ -> return []
        ),
        ("either-for", \ as -> case as of
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
        ("parser-for", \ as -> case as of
            (_, EMonadGens gs _) : (_, EOpen b) : _ -> return [
                Just $ SigMonad SM{
                    gsunit = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_unit", HSVar "gsparser_unit"),
                    gsbind = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_bind", HSVar "gsparser_bind"),
                    gsmap = (Set.singleton $ HSIVar "GSI.Parser" "gsparser_map", HSVar "gsparser_map")
                },
                Just $ SigOpen $ gensBoundVars $ map (\ ( _, g) -> g) gs
              ]
            _ -> return []
        )
    ]
  }

data Env = Env {
    gsimplicits :: Map String [Implicit],
    gssignatures :: Map String ([(Pos, Expr)] -> Compiler [Maybe Signature]),
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

sigCode :: Signature -> String
sigCode s = s `seq` $gsfatal "sigCode next"
