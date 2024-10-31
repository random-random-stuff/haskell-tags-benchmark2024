{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Cookie
    ( -- * Server to client
      -- ** Data type
      SetCookie
    , setCookieName
    , setCookieValue
    , setCookiePath
    , setCookieExpires
    , setCookieMaxAge
    , setCookieDomain
    , setCookieHttpOnly
    , setCookieSecure
    , setCookieSameSite
    , SameSiteOption
    , sameSiteLax
    , sameSiteStrict
    , sameSiteNone
      -- ** Functions
    , parseSetCookie
    , renderSetCookie
    , renderSetCookieBS
    , defaultSetCookie
    , def
      -- * Client to server
    , Cookies
    , parseCookies
    , renderCookies
    , renderCookiesBS
      -- ** UTF8 Version
    , CookiesText
    , parseCookiesText
    , renderCookiesText
      -- * Expires field
    , expiresFormat
    , formatCookieExpires
    , parseCookieExpires
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Char (toLower, isDigit)
import Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString)
import Data.ByteString.Builder.Extra (byteStringCopy)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mempty, mappend, mconcat)
#endif
import Data.Word (Word8)
import Data.Ratio (numerator, denominator)
import Data.Time (UTCTime (UTCTime), toGregorian, fromGregorian, formatTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Control.Arrow (first, (***))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Maybe (isJust)
import Data.Default.Class (Default (def))
import Control.DeepSeq (NFData (rnf))

-- | Textual cookies. Functions assume UTF8 encoding.
type CookiesText = [(Text, Text)]

parseCookiesText :: S.ByteString -> CookiesText
parseCookiesText =
    map (go *** go) . parseCookies
  where
    go = decodeUtf8With lenientDecode

renderCookiesText :: CookiesText -> Builder
renderCookiesText = renderCookiesBuilder . map (encodeUtf8Builder *** encodeUtf8Builder)

type Cookies = [(S.ByteString, S.ByteString)]

-- | Decode the value of a \"Cookie\" request header into key/value pairs.
parseCookies :: S.ByteString -> Cookies
parseCookies s
  | S.null s = []
  | otherwise =
    let (x, y) = breakDiscard 59 s -- semicolon
     in parseCookie x : parseCookies y

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
        value' = dropEnds 34 value -- double quote
     in (key', value')

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

dropEnds :: Word8 -> S.ByteString -> S.ByteString
dropEnds w s =
    case S.unsnoc s of
        Just (s', w') | w' == w -> case S.uncons s' of
            Just (w'', s'') | w'' == w -> s''
            _ -> s
        _ -> s

type CookieBuilder = (Builder, Builder)

renderCookiesBuilder :: [CookieBuilder] -> Builder
renderCookiesBuilder [] = mempty
renderCookiesBuilder cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` char8 ';' `mappend` y

renderCookie :: CookieBuilder -> Builder
renderCookie (k, v) = k `mappend` char8 '=' `mappend` v

renderCookies :: Cookies -> Builder
renderCookies = renderCookiesBuilder . map (byteString *** byteString)

-- | @since 0.4.6
renderCookiesBS :: Cookies -> S.ByteString
renderCookiesBS = L.toStrict . toLazyByteString . renderCookies

-- | Data type representing the key-value pair to use for a cookie, as well as configuration options for it.
--
-- ==== Creating a SetCookie
--
-- 'SetCookie' does not export a constructor; instead, use 'defaultSetCookie' and override values (see <http://www.yesodweb.com/book/settings-types> for details):
--
-- @
-- import Web.Cookie
-- :set -XOverloadedStrings
-- let cookie = 'defaultSetCookie' { 'setCookieName' = "cookieName", 'setCookieValue' = "cookieValue" }
-- @
--
-- ==== Cookie Configuration
--
-- Cookies have several configuration options; a brief summary of each option is given below. For more information, see <http://tools.ietf.org/html/rfc6265#section-4.1.2 RFC 6265> or <https://en.wikipedia.org/wiki/HTTP_cookie#Cookie_attributes Wikipedia>.
data SetCookie = SetCookie
    { setCookieName :: S.ByteString -- ^ The name of the cookie. Default value: @"name"@
    , setCookieValue :: S.ByteString -- ^ The value of the cookie. Default value: @"value"@
    , setCookiePath :: Maybe S.ByteString -- ^ The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
    , setCookieExpires :: Maybe UTCTime -- ^ The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
    , setCookieMaxAge :: Maybe DiffTime -- ^ The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
    , setCookieDomain :: Maybe S.ByteString -- ^ The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
    , setCookieHttpOnly :: Bool -- ^ Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
    , setCookieSecure :: Bool -- ^ Instructs the browser to only send the cookie over HTTPS. Default value: @False@
    , setCookieSameSite :: Maybe SameSiteOption -- ^ The "same site" policy of the cookie, i.e. whether it should be sent with cross-site requests. Default value: @Nothing@
    }
    deriving (Eq, Show)

-- | Data type representing the options for a <https://tools.ietf.org/html/draft-west-first-party-cookies-07#section-4.1 SameSite cookie>
data SameSiteOption = Lax
                    | Strict
                    | None
                    deriving (Show, Eq)

instance NFData SameSiteOption where
  rnf x = x `seq` ()

-- | Directs the browser to send the cookie for <https://tools.ietf.org/html/rfc7231#section-4.2.1 safe requests> (e.g. @GET@), but not for unsafe ones (e.g. @POST@)
sameSiteLax :: SameSiteOption
sameSiteLax = Lax

-- | Directs the browser to not send the cookie for /any/ cross-site request, including e.g. a user clicking a link in their email to open a page on your site.
sameSiteStrict :: SameSiteOption
sameSiteStrict = Strict

-- |
-- Directs the browser to send the cookie for cross-site requests.
--
-- @since 0.4.5
sameSiteNone :: SameSiteOption
sameSiteNone = None

instance NFData SetCookie where
    rnf (SetCookie a b c d e f g h i) =
        a `seq`
        b `seq`
        rnfMBS c `seq`
        rnf d `seq`
        rnf e `seq`
        rnfMBS f `seq`
        rnf g `seq`
        rnf h `seq`
        rnf i
      where
        -- For backwards compatibility
        rnfMBS Nothing = ()
        rnfMBS (Just bs) = bs `seq` ()

-- | @'def' = 'defaultSetCookie'@
instance Default SetCookie where
    def = defaultSetCookie

-- | A minimal 'SetCookie'. All fields are 'Nothing' or 'False' except @'setCookieName' = "name"@ and @'setCookieValue' = "value"@. You need this to construct a 'SetCookie', because it does not export a constructor. Equivalently, you may use 'def'.
--
-- @since 0.4.2.2
defaultSetCookie :: SetCookie
defaultSetCookie = SetCookie
    { setCookieName     = "name"
    , setCookieValue    = "value"
    , setCookiePath     = Nothing
    , setCookieExpires  = Nothing
    , setCookieMaxAge   = Nothing
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = False
    , setCookieSecure   = False
    , setCookieSameSite = Nothing
    }

renderSetCookie :: SetCookie -> Builder
renderSetCookie sc = mconcat
    [ byteString (setCookieName sc)
    , char8 '='
    , byteString (setCookieValue sc)
    , case setCookiePath sc of
        Nothing -> mempty
        Just path -> byteStringCopy "; Path="
                     `mappend` byteString path
    , case setCookieExpires sc of
        Nothing -> mempty
        Just e -> byteStringCopy "; Expires=" `mappend`
                  byteString (formatCookieExpires e)
    , case setCookieMaxAge sc of
        Nothing -> mempty
        Just ma -> byteStringCopy"; Max-Age=" `mappend`
                   byteString (formatCookieMaxAge ma)
    , case setCookieDomain sc of
        Nothing -> mempty
        Just d -> byteStringCopy "; Domain=" `mappend`
                  byteString d
    , if setCookieHttpOnly sc
        then byteStringCopy "; HttpOnly"
        else mempty
    , if setCookieSecure sc
        then byteStringCopy "; Secure"
        else mempty
    , case setCookieSameSite sc of
        Nothing -> mempty
        Just Lax -> byteStringCopy "; SameSite=Lax"
        Just Strict -> byteStringCopy "; SameSite=Strict"
        Just None -> byteStringCopy "; SameSite=None"
    ]

-- | @since 0.4.6
renderSetCookieBS :: SetCookie -> S.ByteString
renderSetCookieBS = L.toStrict . toLazyByteString . renderSetCookie

parseSetCookie :: S.ByteString -> SetCookie
parseSetCookie a = SetCookie
    { setCookieName = name
    , setCookieValue = dropEnds 34 value -- double quote
    , setCookiePath = lookup "path" flags
    , setCookieExpires =
        lookup "expires" flags >>= parseCookieExpires
    , setCookieMaxAge =
        lookup "max-age" flags >>= parseCookieMaxAge
    , setCookieDomain = lookup "domain" flags
    , setCookieHttpOnly = isJust $ lookup "httponly" flags
    , setCookieSecure = isJust $ lookup "secure" flags
    , setCookieSameSite = case lookup "samesite" flags of
        Just "Lax" -> Just Lax
        Just "Strict" -> Just Strict
        Just "None" -> Just None
        _ -> Nothing
    }
  where
    pairs = map (parsePair . dropSpace) $ S.split 59 a ++ [S8.empty] -- 59 = semicolon
    (name, value) = head pairs
    flags = map (first (S8.map toLower)) $ tail pairs
    parsePair = breakDiscard 61 -- equals sign
    dropSpace = S.dropWhile (== 32) -- space

expiresFormat :: String
expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> S.ByteString
formatCookieExpires =
    S8.pack . formatTime defaultTimeLocale expiresFormat

parseCookieExpires :: S.ByteString -> Maybe UTCTime
parseCookieExpires =
    fmap fuzzYear . parseTimeM True defaultTimeLocale expiresFormat . S8.unpack
  where
    -- See: https://github.com/snoyberg/cookie/issues/5
    fuzzYear orig@(UTCTime day diff)
        | x >= 70 && x <= 99 = addYear 1900
        | x >= 0 && x <= 69 = addYear 2000
        | otherwise = orig
      where
        (x, y, z) = toGregorian day
        addYear x' = UTCTime (fromGregorian (x + x') y z) diff

-- | Format a 'DiffTime' for a cookie.
formatCookieMaxAge :: DiffTime -> S.ByteString
formatCookieMaxAge difftime = S8.pack $ show (num `div` denom)
  where rational = toRational difftime
        num = numerator rational
        denom = denominator rational

parseCookieMaxAge :: S.ByteString -> Maybe DiffTime
parseCookieMaxAge bs
  | all isDigit unpacked = Just $ secondsToDiffTime $ read unpacked
  | otherwise = Nothing
  where unpacked = S8.unpack bs
