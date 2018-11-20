{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.ByteStringSpec (main, spec) where

import Control.Monad (guard)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Text.Lazy (Text)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Application)
import Test.Hspec
    ( Spec
    , SpecWith
    , hspec
    , describe
    , it
    )
import Test.Hspec.Wai
    ( MatchHeader (MatchHeader)
    , WaiExpectation
    , get
    , matchHeaders
    , shouldRespondWith
    , with
    , (<:>)
    )
import Test.Hspec.Wai.Matcher (formatHeader)
import qualified Web.Scotty.Trans as ScottyT (ActionT, scottyAppT, get)

import Web.Scotty.ByteString
    ( addHeader
    , setHeader
    , html
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    htmlSpec
    addHeaderSpec
    setHeaderSpec

htmlSpec :: Spec
htmlSpec = describe "html" $ withApp (pure ()) $
    it "responds properly" $
        shouldRespondWithHeader "Content-Type" "text/html; charset=utf-8"

addHeaderSpec :: Spec
addHeaderSpec = describe "addHeader" $ withApp app $ do
    it "adds Header" $
        shouldRespondWithHeader "yo" "dude"

    it "adds duplicate Header" $
        shouldRespondWithHeader "yo" "yo"
  where
    app = do
        addHeader "yo" "dude"
        addHeader "yo" "yo"

setHeaderSpec :: Spec
setHeaderSpec = describe "setHeader" $ withApp app $ do
    it "overwrites first Header" $
        shouldNotRespondWithHeader "yo" "dude"

    it "sets second Header" $
        shouldRespondWithHeader "yo" "yo"
  where
    app = do
        setHeader "yo" "dude"
        setHeader "yo" "yo"

withApp :: ScottyT.ActionT Text IO () -> SpecWith Application -> Spec
withApp = with
        . ScottyT.scottyAppT id
        . ScottyT.get "/"
        . (<*) (html ("hello-world" :: BL.ByteString))

shouldRespondWithHeader :: HeaderName -> BS.ByteString -> WaiExpectation
shouldRespondWithHeader name value =
    shouldRespondWith
        (get "/")
        "hello-world" { matchHeaders = [name <:> value] }

shouldNotRespondWithHeader :: HeaderName -> BS.ByteString -> WaiExpectation
shouldNotRespondWithHeader name value =
    shouldRespondWith
        (get "/")
        "hello-world" { matchHeaders = [matcher name value] }
  where
    matcher n v = MatchHeader $ \hs _ ->
        guard ((n, v) `elem` hs) >> (Just . unlines)
            [ "should not contain header:"
            , formatHeader (n, v)
            ]
