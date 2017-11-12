{-# LANGUAGE QuasiQuotes #-}

module FakeWreq where

import Heredoc (heredoc)

import Control.Lens
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy as BL

data Response a = Response a

get :: String -> IO (Response ByteString)
get = pure . Response . BL.fromStrict . C.pack . fakeContent

fakeContent :: String -> String
fakeContent _ = [heredoc|<html>
  <head>
    <title>Some fake content</title>
  </head>
  <body>
    <heading>A heading</heading>
    <article>The main document is here</article>
  </body>
</html>|]

-- responseBody :: Functor f => (a -> f a) -> Response a -> f (Response a)
responseBody :: Lens' (Response a) a
responseBody inj (Response a) = Response <$> inj a
