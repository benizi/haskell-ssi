{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Control.Exception as Ex
import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Default (Default(..))
import Data.List (dropWhile, intercalate, takeWhile)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing, maybeToList)
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utc, utcToLocalTime)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.FilePath ((</>), isRelative, makeRelative, normalise)
import Text.HTML.TagSoup (Tag(..), parseTags, renderTags)
import Text.Megaparsec
  ( Dec
  , ErrorItem(Tokens)
  , ShowToken(showTokens)
  , SourcePos(SourcePos)
  , Stream
  , Token(..)
  , (<?>)
  , (<|>)
  , between
  , char
  , choice
  , many
  , manyTill
  , noneOf
  , parse
  , parseMaybe
  , satisfy
  , skipMany
  , some
  , string
  , unsafePos
  )
import Text.Megaparsec.Char (alphaNumChar, anyChar, spaceChar)
import qualified Text.Megaparsec.Expr as TME
import Text.Megaparsec.Prim (MonadParsec(..))
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import Text.StringLike (StringLike(..), castString, fromString, toString)
import qualified Text.StringLike as Str

isSsiTag :: StringLike str => Tag str -> Bool
isSsiTag (TagComment c) = longEnough c && startsWithHash c
  where
    longEnough = (1 <) . B.length . castString
    startsWithHash = (== "#") . B.take 1 . castString
isSsiTag _ = False

findSsiTags :: StringLike str => str -> [Tag str]
findSsiTags = filter isSsiTag . parseTags

getVal :: StringLike str => str -> IO str
getVal var = lookupEnv (toString var) >>= pure . maybe Str.empty fromString

data SsiComment = SsiComment
  { ssicDirective :: String
  , ssicAttributes :: M.Map String String
  } deriving (Show)

fromTag :: Tag String -> SsiTokenS
fromTag t@(TagComment c) = either literal tokenized parsed
    where
      parsed = parse ssiComment "" c
      wrapped = TokLiteral t
      literal = const wrapped
      tokenized s = maybe wrapped id $ fromComment s
fromTag t = TokLiteral t

skipws :: Parser ()
skipws = skipMany spaceChar

directive :: Parser String
directive = char '#' >> many alphaNumChar

keyval :: Parser (String, String)
keyval = do
  k <- alphaNumChar `manyTill` char '='
  v <- quote >> quoted `manyTill` quote
  return (k, v)
    where
      quote = char '"'
      escChar = char '\\' >> quote >> return '"'
      quoted = escChar <|> anyChar

ssiComment :: Parser SsiComment
ssiComment = do
  name <- directive <* skipws
  kvpairs <- many (keyval <* skipws)
  return SsiComment
    { ssicDirective = name
    , ssicAttributes = M.fromList kvpairs
    }

data SsiToken str = TokLiteral (Tag str)
                  | TokIf str
                  | TokElse
                  | TokEndif
                  | TokTime str
                  | TokEcho str
                  | TokSet str str
                  | TokIncFile str
                  | TokIncVirtual str
                  deriving (Show, Ord, Eq)

fromComment :: StringLike str => SsiComment -> Maybe (SsiToken str)
fromComment SsiComment { ssicDirective = dir, ssicAttributes = attrs }
  = fromDirAttrs dir $ M.toAscList attrs
    where
      fromDirAttrs :: StringLike str
                   => String
                   -> [(String, String)]
                   -> Maybe (SsiToken str)
      fromDirAttrs "if" [("expr",expr)] = Just $ TokIf $ fromString expr
      fromDirAttrs "else" _ = Just $ TokElse
      fromDirAttrs "endif" _ = Just $ TokEndif
      fromDirAttrs "config" [("timefmt",v)] = Just $ TokTime $ fromString v
      fromDirAttrs "echo" [("var", v)] = Just $ TokEcho $ fromString v
      fromDirAttrs "set" [("value",val),("var",var)] =
        Just $ TokSet (fromString var) (fromString val)
      fromDirAttrs "include" [("file",file)] =
        Just $ TokIncFile (fromString file)
      fromDirAttrs "include" [("virtual",v)] =
        Just $ TokIncVirtual (fromString v)
      fromDirAttrs _ _ = Nothing

type SsiTokenS = SsiToken String

data SsiStream = SsiStreamL [SsiTokenS]

data SsiExprL = SsiExprL [SsiExpr String]

instance Stream SsiStream where
  type Token SsiStream = SsiTokenS
  uncons (SsiStreamL []) = Nothing
  uncons (SsiStreamL (t:ts)) = Just (t, SsiStreamL ts)
  updatePos _ _ apos@(SourcePos n l c) _ =
    (apos, SourcePos n l (c Semigroup.<> unsafePos 1))

expression :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
expression = do
  choice [ simpleTag >>= pure . (: [])
         , ifElseEnd
         ]

expressions :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
expressions = mconcat <$> many expression

simpleTag :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m (SsiExpr String)
simpleTag = satisfy' (isJust . simpleExpr) >>= pure . fromJust . simpleExpr

simpleTags :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
simpleTags = many simpleTag

ifElseEnd :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
ifElseEnd = do
  TokIf expr <- parseIf
  ifBlock <- simpleTags
  elseBlock <- parseElse <|> parseEnd
  return [IfElse expr ifBlock elseBlock]
    where
      parseIf :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m SsiTokenS
      parseIf = satisfy' isIf
        where
          isIf (TokIf _) = True
          isIf _ = False
      parseElse :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
      parseElse = satisfy' (== TokElse) >> simpleTags <* parseEnd
      parseEnd :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
      parseEnd = satisfy' (== TokEndif) >> return mempty

simpleExpr :: SsiTokenS -> Maybe (SsiExpr String)
simpleExpr (TokIf _) = Nothing
simpleExpr TokElse = Nothing
simpleExpr TokEndif = Nothing
simpleExpr tok = convert tok
  where
    convert (TokLiteral tag) = Just $ Literal tag
    convert (TokTime a) = Just $ TimeFormat a
    convert (TokEcho a) = Just $ EchoVar a
    convert (TokSet a b) = Just $ SetVar a b
    convert (TokIncFile a) = Just $ IncludeFile a
    convert (TokIncVirtual a) = Just $ IncludeVirtual a
    convert _ = Nothing

satisfy' :: (Token s ~ SsiTokenS, MonadParsec e s m) => (SsiTokenS -> Bool) -> m SsiTokenS
satisfy' f = token tst Nothing
  where
    tst tok =
      if f tok
         then Right tok
         else Left (Set.singleton (Tokens (tok:|[])), Set.empty, Set.empty)

data SsiExpr str = Literal (Tag str)
                 | TimeFormat str
                 | EchoVar str
                 | SetVar str str
                 | IfElse str [SsiExpr str] [SsiExpr str]
                 | IncludeVirtual str
                 | IncludeFile FilePath
                 deriving (Show)

type SsiVars = M.Map String String

(~>) :: StringLike str => str -> str -> Bool
haystackS ~> needleS =
  let haystack = castString haystackS
      needle = castString needleS
   in needle == BL.take (BL.length needle) haystack

getExpressionsMaybe :: StringLike str => str -> Maybe [SsiExpr String]
getExpressionsMaybe = parseMaybe expressions . tokenStream

tokenStream :: StringLike str => str -> SsiStream
tokenStream = SsiStreamL . fmap fromTag . parseTags . toString

instance ShowToken SsiTokenS where
  showTokens = concatMap show

data SsiEnvironment = SsiEnvironment
  { ssiVars :: SsiVars
  , ssiTimeFormat :: String
  , ssiCurrentTime :: UTCTime
  , ssiTimeZone :: TimeZone
  , ssiBaseUrl :: String
  , ssiRootDir :: FilePath
  , ssiCurrentDir :: FilePath
  , ssiOutput :: [String]
  } deriving (Show)

instance Default SsiEnvironment where
  def = SsiEnvironment
    { ssiVars = M.empty
    , ssiTimeFormat = fromString "%F@%T"
    , ssiCurrentTime = posixSecondsToUTCTime 0
    , ssiTimeZone = utc
    , ssiBaseUrl = fromString ""
    , ssiRootDir = "/"
    , ssiCurrentDir = "/"
    , ssiOutput = []
    }

withEnvVars :: SsiEnvironment -> IO SsiEnvironment
withEnvVars env =
  let orig = ssiVars env
   in do
     vars <- fmap M.fromList getEnvironment
     return env { ssiVars = M.union vars orig }

ssiAddOutput :: String -> SsiEnvironment -> SsiEnvironment
ssiAddOutput out env =
  let outs = ssiOutput env
   in env { ssiOutput = outs ++ [out] }

evalAll :: StringLike str => SsiEnvironment -> [SsiExpr str] -> IO SsiEnvironment
evalAll e [] = pure e
evalAll e (t:ts) = do
  e' <- evalTagged e t
  evalAll e' ts

evalTagged :: StringLike str
           => SsiEnvironment
           -> SsiExpr str
           -> IO SsiEnvironment
evalTagged e' x' = pure (addTag x' e') >>= eval x' >>= pure . endTag x'
  where
    htmlcomment :: String -> String
    htmlcomment txt = renderTags [br, TagComment txt, br]
      where br = TagText "\n"

    addComments :: StringLike str => [str] -> SsiEnvironment -> SsiEnvironment
    addComments out env@SsiEnvironment { ssiOutput = outs } =
      env { ssiOutput = outs ++ (htmlcomment . toString <$> out) }

    addTag :: StringLike str => SsiExpr str -> SsiEnvironment -> SsiEnvironment
    addTag x@(IfElse inner' _ _) env =
      let tf = fromBool $ evalInnerExprBool env $ safeParseInnerExpr inner'
       in addComments ((maybeToList $ fn x) ++ [tf]) env
    addTag expr env = addComments (maybeToList $ fn expr) env

    endTag :: StringLike str => SsiExpr str -> SsiEnvironment -> SsiEnvironment
    endTag (TimeFormat _) env = env
    endTag (SetVar _ _) env = env
    endTag expr env =
      if isNothing $ fn expr
         then env
         else addComments ["/" ++ ctor expr] env

    functiony :: StringLike str => String -> [str] -> String
    functiony name args = concat [name, "(", commaSep args, ")"]

    commaSep :: StringLike str => [str] -> String
    commaSep args = intercalate "," $ toString <$> args

    ctor :: StringLike str => SsiExpr str -> String
    ctor expr =
      case expr of
        Literal _ -> "Literal"
        TimeFormat _ -> "TimeFormat"
        EchoVar _ -> "EchoVar"
        SetVar _ _ -> "SetVar"
        IfElse _ _ _ -> "IfElse"
        IncludeVirtual _ -> "IncludeVirtual"
        IncludeFile _ -> "IncludeFile"

    fn :: StringLike str => SsiExpr str -> Maybe String
    fn expr =
      let args' :: StringLike str => [str] -> Maybe String
          args' = Just . functiony (ctor expr)
       in case expr of
            Literal _ -> Nothing
            TimeFormat fmt -> args' [fmt]
            EchoVar var -> args' [var]
            SetVar var val -> args' [var, val]
            IfElse expr' _ _ -> args' [expr']
            IncludeVirtual virt -> args' [toString virt]
            IncludeFile file -> args' [toString file]

evalTimeInZone :: StringLike str => SsiEnvironment -> TimeZone -> str
evalTimeInZone env zone =
  let time = utcToLocalTime zone $ ssiCurrentTime env
      fmt = ssiTimeFormat env
      locale = defaultTimeLocale
   in fromString $ formatTime locale fmt time

evalVar :: StringLike str => SsiEnvironment -> str -> str
evalVar env "DATE_LOCAL" = evalTimeInZone env $ ssiTimeZone env
evalVar env "DATE_GMT" = evalTimeInZone env utc
evalVar env name =
  let key = toString name
      val = M.lookup key $ ssiVars env
   in fromString $ maybe "" id val

eval :: StringLike str => SsiExpr str -> SsiEnvironment -> IO SsiEnvironment

-- Literal [Tag str]
eval (Literal tag) env@SsiEnvironment { ssiOutput = outs } =
  pure env { ssiOutput = outs ++ rendered }
    where rendered = [toString $ renderTags [tag]]

-- TimeFormat str
eval (TimeFormat f) env = pure env { ssiTimeFormat = toString f }

-- EchoVar str
eval (EchoVar name) env =
  let val = toString $ evalVar env name
   in pure $ ssiAddOutput val env

-- SetVar str str
eval (SetVar name val) env =
  pure env { ssiVars = M.insert name' val' vars }
    where
      vars = ssiVars env
      name' = toString name
      val' = toString val

-- IfElse str [SsiTag str] [SsiTag str]
eval (IfElse expr' ts fs) env =
  let expr = safeParseInnerExpr expr'
      branch = if evalInnerExprBool env expr then ts else fs
   in evalAll env branch

-- IncludeVirtual str
eval (IncludeVirtual expr') env = do
  let expr = safeParseInnerExpr ("\"" ++ (toString expr') ++ "\"")
      val = evalInnerExpr env expr
      cmt = renderTags [TagComment val]
   in pure $ ssiAddOutput cmt env

-- IncludeFile FilePath
eval (IncludeFile file) env =
  let outs = ssiOutput env
      root = ssiRootDir env
      curr = ssiCurrentDir env
   in do
     contents <- fetchLocal root curr $ toString file
     return env { ssiOutput = outs ++ [contents] }

fetchLocal :: FilePath -> FilePath -> String -> IO String
fetchLocal root curr path = either blank id <$> Ex.try slurp
  where
    blank = const "" :: Ex.SomeException -> String
    normal = normalise path
    toresolve =
      if isRelative normal
         then curr </> normal
         else root </> makeRelative "/" normal
    testAllowable canon = do
      if isRelative (makeRelative root canon)
         then return ()
         else fail "Not allowed to fetch files outside root"
    slurp = do
      canon <- canonicalizePath toresolve
      testAllowable canon
      toString <$> C.readFile canon

data InnerExpr = InnerBool Bool
               | InnerLiteral String
               | InnerGetVar String
               | InnerQuotSeq [InnerExpr]
               | InnerUnary InnerUnaryOp InnerExpr
               | InnerBinary InnerBinaryOp InnerExpr InnerExpr
               deriving (Show, Eq, Ord)

pprint' :: (InnerExpr -> Maybe String) -> InnerExpr -> String
pprint' evaluator = unlines . pp ""
  where
    pp :: String -> InnerExpr -> [String]
    pp indent expr =
      let outl l = prepend expr [indent ++ l]
          out l v = outl $ unwords [l, v]
          outs l v = out l $ show v
          recur = pp (indent ++ "  ")
      in case expr of
        InnerBool b -> outs "Bool" b
        InnerLiteral l -> out "Literal" l
        InnerGetVar v -> out "Get" ("$" ++ v)
        InnerQuotSeq [x] -> recur x
        InnerQuotSeq xs -> outl "QuotSeq" ++ concatMap recur xs
        InnerUnary op x -> outs "Unary" op ++ recur x
        InnerBinary op xa xb -> outs "Binary" op ++ recur xa ++ recur xb
    prepend :: InnerExpr -> [String] -> [String]
    prepend in' orig@(x:xs) =
      let ind = takeWhile (== ' ') x
          rst = dropWhile (== ' ') x
          addval val = (ind ++ "[" ++ val ++ "] == " ++ rst) : xs
       in maybe orig addval $ evaluator in'
    prepend _ o = o

pprint :: InnerExpr -> String
pprint = pprint' (const Nothing)

data InnerUnaryOp = Not'
  deriving (Show, Eq, Ord)

data InnerBinaryOp = And' | Or' | Eq'
  deriving (Show, Eq, Ord)

(&&&) :: InnerExpr -> InnerExpr -> InnerExpr
(&&&) = InnerBinary And'

(|||) :: InnerExpr -> InnerExpr -> InnerExpr
(|||) = InnerBinary Or'

not' :: InnerExpr -> InnerExpr
not' = InnerUnary Not'

(/==) :: InnerExpr -> InnerExpr -> InnerExpr
(/==) = (InnerUnary Not' .) . InnerBinary Eq'

(===) :: InnerExpr -> InnerExpr -> InnerExpr
(===) = InnerBinary Eq'

innerExpr :: Parser InnerExpr
innerExpr = TME.makeExprParser term ops <?> "inner expression"
  where
    term = choice [ parenthesized innerExpr, varExpr, quotedString ]

    symbol = L.symbol skipws
    opMaker = fmap . uncurry
    prefix = opMaker $ \tok fn -> TME.Prefix (fn <$ symbol tok <?> tok)
    binary = opMaker $ \tok fn -> TME.InfixL (fn <$ symbol tok <?> tok)
    ops = [ prefix [ ("!", not') ]
          , binary [ ("!=", (/==)), ("=", (===)) ]
          , binary [ ("||", (|||)) ]
          ]

    parenthesized = between (symbol "(") (symbol ")") -- TODO: support nesting
    identifierChar = alphaNumChar <|> char '_'
    identifier = many identifierChar
    varExpr = varAccess <* skipws
    normalChar = noneOf ("${}\"'\\" :: String)
    inbetween c parser = char c >> parser `manyTill` char c
    stringInside c = inbetween c $ satisfy (/= c)
    stringPart = literal <|> varAccess
    literal = InnerLiteral <$> some normalChar
    varAccess = InnerGetVar <$> between (string "${") (char '}') identifier
    singleQuoted = InnerLiteral <$> stringInside '\''
    doubleQuoted = InnerQuotSeq <$> inbetween '"' stringPart
    quotedString = (singleQuoted <|> doubleQuoted) <* skipws

safeParseInnerExpr :: StringLike str => str -> InnerExpr
safeParseInnerExpr = either nil id . parse innerExpr "" . toString
  where
    nil = const $ InnerBool False

falsey :: String -> Bool
falsey = (`elem` ["", "0", "false", "False"])

truthy :: String -> Bool
truthy = not . falsey

fromBool :: Bool -> String
fromBool True = "true"
fromBool _ = "false"

evalInnerExpr :: SsiEnvironment -> InnerExpr -> String
evalInnerExpr env = eval'
  where
    toBool = truthy . eval'
    boolBinOp f a b = fromBool $ f (toBool a) (toBool b)
    eval' :: InnerExpr -> String
    eval' (InnerBool bool) = fromBool bool
    eval' (InnerLiteral s) = s
    eval' (InnerGetVar name) = evalVar env name
    eval' (InnerQuotSeq parts) = concatMap eval' parts
    eval' (InnerUnary Not' a) = fromBool $ not $ toBool a
    eval' (InnerBinary Eq' a b) = fromBool $ (eval' a) == (eval' b)
    eval' (InnerBinary And' a b) = boolBinOp (&&) a b
    eval' (InnerBinary Or' a b) = boolBinOp (||) a b

evalInnerExprBool :: SsiEnvironment -> InnerExpr -> Bool
evalInnerExprBool = (truthy .) . evalInnerExpr

readExprs :: FilePath -> IO [SsiExpr String]
readExprs filename = B.readFile filename >>= pure . exprStream filename

exprStream :: String -> B.ByteString -> [SsiExpr String]
exprStream name = either mempty id . parse expressions name . tokenStream

fromEnv :: String
        -> String
        -> (SsiEnvironment -> String -> SsiEnvironment)
        -> SsiEnvironment
        ->IO SsiEnvironment
fromEnv key defval setter orig = do
  ret <- lookupEnv key
  let val = maybe defval id ret
   in return $ setter orig val

addVar :: String -> String -> SsiEnvironment -> SsiEnvironment
addVar key val env =
  let vars = ssiVars env
   in env { ssiVars = M.insert key val vars }

createSsiEnvironment :: IO SsiEnvironment
createSsiEnvironment = do
  cwd <- getCurrentDirectory
  foldM (\env modifier -> modifier env) def
    [ fromEnv "SSI_BASE_URL" "https://example.com/" (\env url -> env { ssiBaseUrl = url })
    , fromEnv "SSI_ROOT" cwd (\env root -> env { ssiRootDir = root })
    , fromEnv "SSI_CWD" cwd (\env d -> env { ssiCurrentDir = d })
    , \env -> (getCurrentTime >>= \time -> pure env { ssiCurrentTime = time })
    , \env -> (getCurrentTimeZone >>= \zone -> pure env { ssiTimeZone = zone })
    , withEnvVars
    ]

main :: IO ()
main = do
  (file:_) <- getArgs
  env <- createSsiEnvironment
  stream <- readExprs file
  evalAll env stream >>= mapM_ putStr . ssiOutput
