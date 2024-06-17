module Interpret where
import Parser
import AST
import qualified Value
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.RWS.Lazy
import qualified Data.Map as Map
import Control.Monad.Except
import Data.Fixed

type Environment = Map.Map ByteString Value.Value
type EvalM = RWS () ByteString Environment
data LineInfo = LineInfo { line :: ByteString
                         , lineno :: Int
                         , isEnd :: Bool }

interpretProgram :: AwkStatement -> LineInfo -> EvalM ()
-- only begin matches first line
interpretProgram (AwkStatement BeginExpression actions) l@(LineInfo _ 0 _) = doActions actions l
interpretProgram (AwkStatement BeginExpression _) l = pure ()
interpretProgram s (LineInfo _ 0 _) = pure ()
-- end matches last line (along with other statements)
interpretProgram (AwkStatement EndExpression actions) l@(LineInfo _ _ True) = doActions actions l
interpretProgram (AwkStatement EndExpression _) l = pure ()
-- otherwise interpret as normal
interpretProgram (AwkStatement exp actions) l = do
    b <- interpretExpression exp l
    if Value.toBool b
        then doActions actions l
        else pure ()

doActions :: [Action] -> LineInfo -> EvalM ()
doActions actions l = mapM_ (interpretAction l) actions

tellLine :: ByteString -> EvalM ()
tellLine b = tell $ b <> "\n"

interpretAction :: LineInfo -> Action -> EvalM ()
interpretAction l (PrintAction []) = tellLine $ line l
interpretAction l (PrintAction exps) = do
    expResults <- traverse (\e -> interpretExpression e l) exps
    tellLine $ BS.unwords $ map Value.showVal expResults
interpretAction l (AssignAction var e) = do
    val <- interpretExpression e l
    vars <- get
    put $ Map.insert var val vars
interpretAction l (AssignUpdateAction var op e) = do
    expVal <- interpretExpression e l
    vars <- get
    oldVarVal <- lookupVar var
    let newVarVal = op oldVarVal expVal
    put $ Map.insert var newVarVal vars

interpretExpression :: Expression -> LineInfo -> EvalM Value.Value
interpretExpression (BinaryExpression e1 op e2) l = op <$> interpretExpression e1 l <*> interpretExpression e2 l
interpretExpression (UnaryExpression e op) l = op <$> interpretExpression e l
interpretExpression (FloatExpression f) _ = pure $ Value.FloatVal f
interpretExpression (StringExpression s) _ = pure $ Value.StringVal s
interpretExpression (VarNameExpression var) _ = lookupVar var
interpretExpression (ColvarExpression e) l = do
    val <- interpretExpression e l
    pure $ evalColvar val l
interpretExpression (NRExpression) l = pure $ Value.FloatVal $ fromIntegral $ lineno l
interpretExpression (NFExpression) l = pure $ Value.FloatVal $ fromIntegral $ length $ BS.words $ line l

evalColvar :: Value.Value -> LineInfo -> Value.Value
evalColvar v l =
    if intVal == 0
        then Value.StringVal $ line l
        else if intVal <= len
            then Value.StringVal $ ws !! (intVal - 1)
            else Value.StringVal ""
    where
        ws = BS.words $ line l
        len = length ws
        intVal = truncate $ Value.toFloat v

lookupVar :: ByteString -> EvalM Value.Value
lookupVar varName = do
    vars <- get
    case Map.lookup varName vars of
        Just f -> pure f
        Nothing -> pure $ Value.FloatVal 0

