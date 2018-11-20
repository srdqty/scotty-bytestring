{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.ByteString
    ( header
    , headers
    , addHeader
    , setHeader
    , html
    ) where

import Control.Monad.State (modify)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.CaseInsensitive as CI (mk)
import Network.HTTP.Types (Header, HeaderName)
import Network.Wai (requestHeaders)
import Web.Scotty.Internal.Types
    ( ActionT (ActionT)
    , ScottyResponse (srHeaders)
    )
import Web.Scotty.Trans (ScottyError, raw, request)

header :: (ScottyError e, Monad m)
       => BS.ByteString
       -> ActionT e m (Maybe BS.ByteString)
header key = lookup (CI.mk key) . requestHeaders <$> request

headers :: (ScottyError e, Monad m) => ActionT e m [Header]
headers = requestHeaders <$> request

addHeader :: Monad m => BS.ByteString -> BS.ByteString -> ActionT e m ()
addHeader = changeHeader add

setHeader :: Monad m => BS.ByteString -> BS.ByteString -> ActionT e m ()
setHeader = changeHeader replace

html :: (ScottyError e, Monad m) => BL.ByteString -> ActionT e m ()
html bytes = do
    changeHeader addIfNotPresent "Content-Type" "text/html; charset=utf-8"
    raw bytes

-- Based on the same function in Web.Scotty.Action, but changed to work with
-- BS.ByteString
changeHeader :: Monad m
             => (HeaderName -> BS.ByteString -> [Header] -> [Header])
             -> BS.ByteString
             -> BS.ByteString
             -> ActionT e m ()
changeHeader f k = ActionT . modify . setHeaderWith . f (CI.mk k)

-- Copy of the same (unexported) function in Web.Scotty.Util
setHeaderWith :: ([Header] -> [Header]) -> ScottyResponse -> ScottyResponse
setHeaderWith f sr = sr { srHeaders = f (srHeaders sr) }

-- Copy of the same (unexported) function in Web.Scotty.Util
replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace k v = add k v . filter ((/= k) . fst)

-- Copy of the same (unexported) function in Web.Scotty.Util
add :: a -> b -> [(a,b)] -> [(a,b)]
add k v m = (k,v):m

-- Copy of the same (unexported) function in Web.Scotty.Util
addIfNotPresent :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
addIfNotPresent k v = go
    where go []         = [(k,v)]
          go l@((x,y):r)
            | x == k    = l
            | otherwise = (x,y) : go r
