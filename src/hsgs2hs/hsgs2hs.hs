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

import HSGS.Parser (Parser, parse, Advanceable(..), advanceStr)
import HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Pattern(..), Generator(..), Param(..), interpolation, quote, scCode, eCode, qloiCode, patCode, genCode)
import HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode)

import qualified HSGS.Syntax as Syntax

main = do
    as <- getArgs
    mapM processArg as

processArg a = do
    id <- doesDirectoryExist a
    irf <- doesFileExist a
    if id then do
        as <- getDirectoryContents a
        mapM_ processArg $ map (\ a' -> a ++ '/' : a') $ filter (\ a' -> a' /= "." && a' /= "..") as
      else if irf && ".hsgs" `isSuffixOf` a then do
        let ?enc = UTF8Strict
        s <- readFile a
        case compileHSGSSource a s of
            Left err -> do
                hPutStrLn stderr err
                exitWith $ ExitFailure 1
            Right s' -> do
                writeFile (mkHSFile a) s'
      else do
        return ()

compileHSGSSource :: FilePath -> String -> Either String String
compileHSGSSource fn s =
   splitInput (Pos fn 1 1) s >>=
   compileSource >>=
   formatOutput >>=
   return . concat . (("{-# LINE 1 " ++ show fn ++ " #-}\n"):)

mkHSFile :: FilePath -> FilePath
mkHSFile ".hsgs" = ".hs"
mkHSFile (c:s) = c:mkHSFile s
mkHSFile "" = ""

formatOutput :: [DestComp] -> Either String [String]
formatOutput (DCChar c:dcs) = ([c]:) <$> formatOutput dcs
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

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource (SCChar c:scs) = (DCChar c:) <$> compileSource scs
compileSource (SCImports:scs) = do
    dcs <- compileSource scs
    return $ DCImports (gatherImports Set.empty dcs) : dcs
compileSource (SCArg pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileArg (processHSVS ps globalEnv) pos e Nothing) <*> compileSource scs
compileSource (SCExpr ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileExpr (processHSVS ps globalEnv) e) <*> compileSource scs
compileSource (SCOpenExpr pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileOpenExpr (processHSVS ps globalEnv) pos (processFVS ps) e) <*> compileSource scs
compileSource (SCOpenArg pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileOpenArg (processHSVS ps globalEnv) pos (processFVS ps) e) <*> compileSource scs
compileSource (SCPat ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePat globalEnv p <*> compileSource scs
compileSource (SCPatArg pos ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePatArg globalEnv pos p <*> compileSource scs
compileSource (SCBody pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBody (processHSVS ps globalEnv) pos e) <*> compileSource scs
compileSource (SCBind pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> runCompiler (compileBind (processHSVS ps globalEnv) pos e) <*> compileSource scs
compileSource (sc:scs) = $gsfatal $ "compileSource " ++ scCode sc ++ " next"
compileSource [] = return []

gatherImports :: Set HSImport -> [DestComp] -> Set HSImport
gatherImports is (DCChar _:dcs) = gatherImports is dcs
gatherImports is (DCExpr is' _:dcs) = gatherImports (is `Set.union` is') dcs
gatherImports is (dc:dcs) = $gsfatal $ "gatherImports " ++ dcCode dc ++ " next"
gatherImports is [] = is

processHSVS :: [Param] -> Env -> Env
processHSVS ps env = env{
    gsvars = Map.fromList [ (v, (Set.empty, HSVar v)) | HSVSParam vs <- ps, v <- vs ] `Map.union` gsvars env
  }

processFVS :: [Param] -> Set String
processFVS ps = Set.fromList [ v | FVSParam vs <- ps, v <- vs ]

compileArg :: Env -> Pos -> Expr -> Maybe Signature -> Compiler (Set HSImport, HSExpr)
compileArg env pos e@EMissingCase{} s = compileExprToArg env pos e
compileArg env pos e@EQLO{} s = compileExprToArg env pos e
compileArg env pos (EVar _ v) s = do
    (isv, ev) <- case Map.lookup v (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    return (
        Set.singleton (HSIType "GSI.Value" "GSArg") `Set.union` isv,
        HSConstr "GSArgVar" `HSApp` ev
      )
compileArg env pos (EPat p) s = lift $ compilePatArg env pos p
compileArg env pos (EOpen e) s = compileOpenArg env pos fvs e where
    fvs = case s of
        Nothing -> Set.empty
        Just (SigOpen vs) -> vs
compileArg env pos (EGens gs pos1) s = compileGensArg env pos gs pos1
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
compileExpr env (EVar pos v) = do
    (isv, ev) <- case Map.lookup v (gsvars env) of
        Nothing -> lift $ Left $ fmtPos pos $ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcenter_w", HSIType "GSI.Util" "Pos" ] `Set.union` isv,
        HSVar "gsbcenter_w" `HSApp` (hspos pos) `HSApp` ev
      )
compileExpr env (EQLO pos "qq" s) = do
    (is, as) <- w s
    return (
        Set.fromList [ HSIVar "GSI.String" "gsbcstring_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcstring_w" `HSApp` hspos pos `HSApp` HSList as
      )
  where
    w [] = return (Set.empty, [])
    w (QChar pos1 ch:qis) = w_ch pos1 (ch:) qis
    w (QInterpExpr pos1 e:qis) = do
        (ise, hse) <- compileArg env pos1 e Nothing
        (ist, hst) <- w qis
        return (ise `Set.union` ist, hse : hst)
    w (qi:qis) = $gsfatal $ "w " ++ qloiCode qi ++ " next"

    w_ch pos ds [] = return (string_imports, [ string_expr pos (ds "") ])
    w_ch pos ds (QChar _ ch:qis) = w_ch pos (ds . (ch:)) qis
    w_ch pos ds (QQChar _ 'n':qis) = w_ch pos (ds . ('\n':)) qis
    w_ch pos ds (QQChar _ ch:qis) = $gsfatal $ "w_ch pos ds (QQChar _ " ++ show ch ++ ":qis) next"
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
compileExpr env (EQLO pos0 q s) = $gsfatal $ "compileExpr (EQLO pos " ++ show q ++ " s) next"
compileExpr env (EApp f pos1 e) = compileApp env f [(pos1, e)]
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
                Set.fromList [ HSIVar "GSI.ByteCode" "gsbcfield_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is0,
                HSVar "gsbcfield_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString v) `HSApp` HSVar "env" `HSApp` (HSLambda [hsv] hse0)
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
compileApp env (EApp f pos a) as = compileApp env f ((pos, a):as)
compileApp env f as = $gsfatal $ "compileApp " ++ eCode f ++ " next"

compilePat :: Env -> Pattern -> Either String (Set HSImport, HSExpr)
compilePat env (PVar pos v) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarpattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcvarpattern_w" `HSApp` hspos pos `HSApp` HSString v
  )
compilePat env (PApp p0 p1) = compilePatApp env p0 [p1]
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
compilePatApp env p as = $gsfatal $ "compilePatApp " ++ patCode p ++ " next"

compileGensArg :: Env -> Pos -> [(Pos, Generator)] -> Pos -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileGensArg env pos gs pos1 = do
    (is, hse) <- compileGens env gs pos1
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileGens :: Env -> [(Pos, Generator)] -> Pos -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileGens env ((pos, g):gs) pos1 = do
    (is, hse) <- compileGenArg env pos g
    (ist, hst) <- compileGens env gs pos1
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbccomposeimpgen_w", HSIType "GSI.Util" "Pos" ] `Set.union` is `Set.union` ist,
        HSVar "gsbccomposeimpgen_w" `HSApp` hspos pos `HSApp` hse `HSApp` (HSLambda ["env"] hst)
      )
compileGens env [] pos1 = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcemptyimpgen_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcemptyimpgen_w" `HSApp` hspos pos1
  )

compileGenArg :: Env -> Pos -> Generator -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileGenArg env pos g = do
    (is, hse) <- compileGen env pos g
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileGen :: Env -> Pos -> Generator -> StateT Integer (Either String) (Set HSImport, HSExpr)
compileGen env pos (ExecGenerator pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpexecbind_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcimpexecbind_w" `HSApp` hspos pos1 `HSApp` hse
      )
compileGen env pos (BindGenerator x pos1 e) = do
    (is, hse) <- compileArg env pos1 e Nothing
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcimpvarbind_w", HSIType "GSI.Util" "Pos", HSIVar "GSI.Syn" "gsvar" ] `Set.union` is,
        HSVar "gsbcimpvarbind_w" `HSApp` hspos pos `HSApp` (HSVar "gsvar" `HSApp` HSString x) `HSApp` hse
      )
compileGen env pos g = $gsfatal $ "compileGen env pos " ++ genCode g ++ " next"

hspos :: Pos -> HSExpr
hspos pos = HSConstr "Pos" `HSApp` HSString (filename pos) `HSApp` HSInteger (line pos) `HSApp` HSInteger (col pos)

splitInput :: Pos -> String -> Either String [SourceComp]
splitInput pos ('$':s) = case parse interpolation pos s of
    Left err -> (SCChar '$':) <$> splitInput (advance '$' pos) s
    Right (r, pos', s') -> (r:) <$> splitInput pos' s'
splitInput pos ('[':'g':'s':':':s) = case parse (quote Syntax.globalEnv pos) (advanceStr "[gs:" pos) s of
    Left err -> error err
    Right (r, pos', '|':']':s') -> (r:) <$> splitInput (advanceStr "|]" pos') s'
    Right (r, pos', s') -> error $ fmtPos pos' $ "Got " ++ show s' ++ "; expected \"|]\""
splitInput pos (c:s) = (SCChar c:) <$> splitInput (advance c pos) s
splitInput pos "" = return []

type Compiler = StateT Integer (Either String)

getGenSym :: Compiler String
getGenSym = state $ \ n -> ("x" ++ show n, n + 1)

runCompiler :: StateT Integer (Either String) a -> Either String a
runCompiler a = evalStateT a 0

globalEnv :: Env
globalEnv = Env{
    gsvars = Map.fromList [
        ("analyze", (Set.singleton $ HSIVar "GSI.StdLib" "gsanalyze", HSVar "gsanalyze")),
        ("case", (Set.singleton $ HSIVar "GSI.StdLib" "gscase", HSVar "gscase")),
        ("error", (Set.singleton $ HSIVar "GSI.StdLib" "gserror", HSVar "gserror")),
        ("file.stat", (Set.singleton $ HSIVar "GSI.Env" "gsfileStat", HSVar "gsfileStat")),
        ("gsv", (Set.singleton $ HSIVar "GSI.Log" "gsloggsv", HSVar "gsloggsv")),
        ("impfor", (Set.singleton $ HSIVar "GSI.StdLib" "gsimpfor", HSVar "gsimpfor")),
        ("print-error", (Set.singleton $ HSIVar "GSI.Env" "gsprintError", HSVar "gsprintError"))
    ],
    gsimplicits = Map.fromList [
        ("error", [ ImHere ])
    ],
    gsviews = Map.fromList [
        ("ENOENT", (Set.singleton $ HSIVar "GSI.Env" "gsENOENT_view", HSVar "gsENOENT_view")),
        ("left", (Set.singleton $ HSIVar "GSI.Either" "gsleft_view", HSVar "gsleft_view")),
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright_view", HSVar "gsright_view"))
    ],
    gssignatures = Map.fromList [
        ("case", \ as -> case as of
            (_, EPat p) : (_, EOpen b) : _ -> return [ Nothing, Just (SigOpen (boundVars p)) ]
            _ -> return []
        )
    ]
  }

data Env = Env {
    gsimplicits :: Map String [Implicit],
    gssignatures :: Map String ([(Pos, Expr)] -> Compiler [Maybe Signature]),
    gsvars :: Map String (Set HSImport, HSExpr),
    gsviews :: Map String (Set HSImport, HSExpr)
  }

boundVars :: Pattern -> Set String
boundVars (PVar _ x) = Set.singleton x
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

sigCode :: Signature -> String
sigCode s = s `seq` $gsfatal "sigCode next"
