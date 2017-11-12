{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Control.Exception as Ex
import Control.Lens ((^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Default (Default(..))
import Data.List (dropWhile, intercalate, takeWhile)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe
  ( catMaybes
  , fromJust
  , isJust
  , isNothing
  , listToMaybe
  , maybeToList
  )
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utc, utcToLocalTime)
import qualified Network.Wreq as Wreq
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.FilePath
  ( (</>)
  , isRelative
  , makeRelative
  , normalise
  , takeDirectory
  )
import System.IO (hFlush, hPutStrLn, stderr)
import Text.HTML.TagSoup (Tag(TagText,TagComment), renderTags)
import Text.Megaparsec
  ( Dec
  , ErrorItem(Tokens)
  , Parsec
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
import Text.Megaparsec.Prim (MonadParsec(notFollowedBy,token))
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import Text.StringLike (StringLike(..), castString, fromString, toString)

data SsiComment = SsiComment
  { ssicDirective :: String
  , ssicAttributes :: M.Map String String
  } deriving (Show, Eq)

skipws :: Parser ()
skipws = skipMany spaceChar

directive :: Parser String
directive = string "<!--#" >> some alphaNumChar

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
  kvpairs <- (keyval <* skipws) `manyTill` string "-->"
  return SsiComment
    { ssicDirective = name
    , ssicAttributes = M.fromList kvpairs
    }

data FileToken = FileLiteral B.ByteString
               | FileSsiComment SsiComment
               deriving (Show, Eq)

type BParser = Parsec Dec B.ByteString

fileToken :: BParser FileToken
fileToken = textliteral <|> comment
  where
    stoppingAt p = notFollowedBy p >> anyChar
    textliteral = FileLiteral <$> C.pack <$> some (stoppingAt ssicBeg)
    ssicBeg = string "<!--#"
    ssicEnd = string "-->"
    comment :: BParser FileToken
    comment = do
      text <- do
        s <- ssicBeg
        i <- many (stoppingAt ssicEnd)
        e <- ssicEnd
        return $ concat [s,i,e]
      case parse ssiComment "" text of
        Left _ -> return (FileLiteral $ C.pack text)
        Right parsed -> return $ FileSsiComment parsed

fileTokens :: BParser [FileToken]
fileTokens = many fileToken

data SsiToken str = TokLiteral str
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

fileContent :: FileToken -> B.ByteString
fileContent (FileLiteral l) = l
fileContent (FileSsiComment c) = C.pack $ concat [start, dir, attrs, end]
  where
    start = "<!--#"
    end = " -->"
    dir = ssicDirective c
    attrs = concatMap kvpair $ M.toList (ssicAttributes c)
    kvpair (k,v) = " " ++ k ++ "=\"" ++ v ++ "\""

fromFileToken :: FileToken -> SsiTokenS
fromFileToken ft =
  case ft of
    FileLiteral l -> TokLiteral $ toString l
    FileSsiComment ssi -> maybe asLiteral id (fromComment ssi)
  where
    asLiteral = TokLiteral $ castString $ fileContent ft

type SsiTokenS = SsiToken String

data SsiStream = SsiStreamL [SsiTokenS]

instance Stream SsiStream where
  type Token SsiStream = SsiTokenS
  uncons (SsiStreamL []) = Nothing
  uncons (SsiStreamL (t:ts)) = Just (t, SsiStreamL ts)
  updatePos _ _ apos@(SourcePos n l c) _ =
    (apos, SourcePos n l (c Semigroup.<> unsafePos 1))

expression :: (e ~ Dec, Token s ~ SsiTokenS, MonadParsec e s m) => m [SsiExpr String]
expression = do
  choice [ simpleTag >>= pure . pure
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
    convert (TokLiteral l) = Just $ Literal $ toString l
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

data SsiExpr str = Literal str
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
tokenStream = SsiStreamL . toexprs . orempty . parsetoks . castString
  where
    toexprs = fmap fromFileToken
    orempty = either mempty id
    parsetoks = parse fileTokens ""

instance ShowToken SsiTokenS where
  showTokens = concatMap show

data SsiEnvironment = SsiEnvironment
  { ssiVars :: SsiVars
  , ssiTimeFormat :: String
  , ssiCurrentTime :: UTCTime
  , ssiTimeZone :: TimeZone
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
    , ssiRootDir = "/"
    , ssiCurrentDir = "/"
    , ssiOutput = []
    }

mergeEnvVars :: SsiEnvironment -> [(String,String)] -> SsiEnvironment
mergeEnvVars env vars = ssiModifyVars env (`M.union` M.fromList vars)

setSsiDirectories :: SsiEnvironment -> String -> SsiEnvironment
setSsiDirectories env realcwd =
  let ssicwd = ssiLookupVar env "SSI_CWD"
      cwd = maybe realcwd id ssicwd
      root = maybe cwd id (ssiLookupVar env "SSI_ROOT")
   in env { ssiRootDir = root, ssiCurrentDir = cwd }

setTimeAndZone :: SsiEnvironment -> UTCTime -> TimeZone -> SsiEnvironment
setTimeAndZone env time zone =
  env { ssiCurrentTime = time, ssiTimeZone = zone }

ssiLookupVar :: SsiEnvironment -> String -> Maybe String
ssiLookupVar = (flip M.lookup) . ssiVars

ssiModifyVars :: SsiEnvironment -> (SsiVars -> SsiVars) -> SsiEnvironment
ssiModifyVars env f = env { ssiVars = f (ssiVars env) }

ssiInsertVar :: SsiEnvironment -> String -> String -> SsiEnvironment
ssiInsertVar env key val = ssiModifyVars env (M.insert key val)

ssiAddOutput :: SsiEnvironment -> String -> SsiEnvironment
ssiAddOutput env out =
  let outs = ssiOutput env
   in env { ssiOutput = outs ++ [out] }

evalAll :: StringLike str => [SsiExpr str] -> SsiEnvironment -> IO SsiEnvironment
evalAll [] e = pure e
evalAll (x:xs) e = eval x e >>= evalAll xs

htmlcomment :: String -> String
htmlcomment txt = renderTags [nl, TagComment $ castString txt, nl]
  where nl = TagText "\n"

evalTagged :: StringLike str
           => SsiEnvironment
           -> SsiExpr str
           -> IO SsiEnvironment
evalTagged e' x' = pure (addTag x' e') >>= eval x' >>= pure . endTag x'
  where
    addComments :: StringLike str => [str] -> SsiEnvironment -> SsiEnvironment
    addComments out env =
      foldl ssiAddOutput env (htmlcomment . toString <$> out)

    addTag :: StringLike str => SsiExpr str -> SsiEnvironment -> SsiEnvironment
    addTag x@(IfElse inner' _ _) env =
      let tf = fromBool $ evalInnerExprBool env $ safeParseInnerExpr inner'
       in addComments ((maybeToList $ prn x) ++ [tf]) env
    addTag expr env = addComments (maybeToList $ prn expr) env

    endTag :: StringLike str => SsiExpr str -> SsiEnvironment -> SsiEnvironment
    endTag (TimeFormat _) env = env
    endTag (SetVar _ _) env = env
    endTag expr env =
      if isNothing $ prn expr
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

    prn :: StringLike str => SsiExpr str -> Maybe String
    prn expr =
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
      val = ssiLookupVar env key
   in fromString $ maybe "" id val

eval :: StringLike str => SsiExpr str -> SsiEnvironment -> IO SsiEnvironment

-- Literal str
eval (Literal str) env =
  pure $ ssiAddOutput env (toString str)

-- TimeFormat str
eval (TimeFormat f) env = pure env { ssiTimeFormat = toString f }

-- EchoVar str
eval (EchoVar name) env =
  let val = toString $ evalVar env name
   in pure $ ssiAddOutput env val

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
   in evalAll branch env

-- IncludeVirtual str
eval (IncludeVirtual expr') env =
  let expr = safeParseInnerExpr ("\"" ++ (toString expr') ++ "\"")
      pathinfo = evalInnerExpr env expr
   in ssiAppendIO env (handleErr <$> fetchRemote env pathinfo)
  where
    handleErr :: HttpResponseE -> String
    handleErr = either id responseBody

-- IncludeFile FilePath
eval (IncludeFile file) env =
  ssiAppendIO env (fetchLocal env file)

ssiAppendIO :: SsiEnvironment -> IO String -> IO SsiEnvironment
ssiAppendIO env toadd = ssiAddOutput env <$> toadd

fetchLocal :: SsiEnvironment -> FilePath -> IO String
fetchLocal env path = do
  filename <- listToMaybe <$> resolveLocalFiles env (Just path)
  case filename of
    Nothing -> return blank
    Just actual -> either giveup id <$> slurp actual
  where
    slurp file = Ex.try (toString <$> C.readFile file)
    blank = "" :: String
    giveup = const blank :: Ex.SomeException -> String

resolveLocalFiles :: SsiEnvironment -> Maybe FilePath -> IO [FilePath]
resolveLocalFiles env first =
  filter allowed <$> fmap translateroot <$> mapM canonicalizePath expanded
  where
    defaults :: [FilePath]
    defaults = normalise <$> catMaybes (first:fromSsiEnv)

    expanded :: [FilePath]
    expanded = catMaybes cross
      where
        cross = [find path | path <- defaults, find <- finders]
        finders = [absolute, Just . resolve]

    absolute :: FilePath -> Maybe FilePath
    absolute path = if isRelative path then Nothing else Just path

    resolveIn :: FilePath -> FilePath -> FilePath
    resolveIn under path =
      if isRelative path
         then current </> path
         else under </> makeRelative "/" path

    resolve :: FilePath -> FilePath
    resolve = resolveIn docroot

    docroot :: FilePath
    docroot = maybe ssiroot id (getenv "DOCUMENT_ROOT")

    current :: FilePath
    current = ssiCurrentDir env

    translateroot :: FilePath -> FilePath
    translateroot path =
      if isRelative path
         then path
         else ssiroot </> makeRelative docroot path

    ssiroot :: FilePath
    ssiroot = ssiRootDir env

    allowed :: FilePath -> Bool
    allowed = isRelative . makeRelative ssiroot

    getenv :: String -> Maybe String
    getenv = ssiLookupVar env

    fromSsiEnv :: [Maybe FilePath]
    fromSsiEnv = getenv <$> words "SCRIPT_FILENAME PATH_TRANSLATED"

type HttpResponse = Wreq.Response BL.ByteString

type HttpResponseE = Either String HttpResponse

getHTTP :: String -> IO HttpResponseE
getHTTP url = Ex.handle toError fetch
  where
    fetch :: IO HttpResponseE
    fetch = Right <$> Wreq.get url
    toError :: Ex.SomeException -> IO HttpResponseE
    toError _ = pure $ Left ("Failed to fetch [" ++ url ++ "]")

responseBody :: HttpResponse -> String
responseBody res = C.unpack $ BL.toStrict $ res ^. Wreq.responseBody

urlFor :: String -> String -> String -> String -> String
urlFor scheme host port path = concat [scheme, "://", host, ":", port, path]

fetchRemote :: StringLike str => SsiEnvironment -> str -> IO HttpResponseE
fetchRemote env reqpathStr = do
  host <- needvar "SERVER_NAME"
  port <- needvar "SERVER_PORT"
  path <- if isRelative reqpath
             then (`makeRelative` reqpath) <$> takeDirectory <$> currpath
             else pure reqpath
  getHTTP (urlFor scheme host port path)
  where
    reqpath :: FilePath
    reqpath = toString reqpathStr
    getvar :: String -> Maybe String
    getvar = ssiLookupVar env
    needvar :: String -> IO String
    needvar var =
      case getvar var of
        Nothing -> fail $ "Missing var " ++ var
        Just val -> return val
    scheme :: String
    scheme = if isJust (getvar "HTTPS") then "https" else "http"
    currpath :: IO String
    currpath = needvar "PATH_INFO"

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

createSsiEnvironment :: IO SsiEnvironment
createSsiEnvironment =
  mergeEnvVarsIO def >>= setSsiDirectoriesIO >>= setTimeAndZoneIO

mergeEnvVarsIO :: SsiEnvironment -> IO SsiEnvironment
mergeEnvVarsIO env = mergeEnvVars env <$> getEnvironment

setSsiDirectoriesIO :: SsiEnvironment -> IO SsiEnvironment
setSsiDirectoriesIO env = setSsiDirectories env <$> getCurrentDirectory

setTimeAndZoneIO :: SsiEnvironment -> IO SsiEnvironment
setTimeAndZoneIO env = do
  time <- getCurrentTime
  zone <- getCurrentTimeZone
  pure $ setTimeAndZone env time zone

findFiles :: SsiEnvironment -> IO [FilePath]
findFiles ssi = do
  fromargs <- getArgs
  fromenv <- resolveLocalFiles ssi Nothing
  return (fromargs ++ fromenv)

findFile :: SsiEnvironment -> IO FilePath
findFile ssi = do
  files <- listToMaybe <$> findFiles ssi
  case files of
    Nothing -> fail "Couldn't determine file to process"
    Just file -> return file

process :: IO ()
process = do
  env <- createSsiEnvironment
  file <- findFile env
  stream <- readExprs file
  result <- evalAll stream env
  mapM_ putStr (ssiOutput result)

main :: IO ()
main = Ex.handle dumpException process
  where
    dumpException :: Ex.SomeException -> IO ()
    dumpException e = do
      enabled <- lookupEnv "DBG"
      if isJust enabled
         then putStrLn (show e)
         else hPutStrLn stderr (show e) >> hFlush stderr
