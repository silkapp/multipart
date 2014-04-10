-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Header
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  Anders Kaseorg <andersk@mit.edu>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing of HTTP headers (name, value pairs)
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Network.CGIMultipart.Header (
                              -- * Headers
                              Headers,
                              HeaderName(..),
                              HeaderValue(..),
                              pHeaders,

                              -- * Content-type
                              ContentType(..),
                              getContentType,
                              parseContentType,
                              showContentType,

                              -- * Content-transfer-encoding
                              ContentTransferEncoding(..),
                              getContentTransferEncoding,

                              -- * Content-disposition
                              ContentDisposition(..),
                              getContentDisposition,

                              -- * Utilities
                              parseM,
                              caseInsensitiveEq,
                              caseInsensitiveCompare,
                              lexeme, ws1, p_token
                          ) where

import Control.Monad
import Data.Char
import Data.List

import Network.CGI.Protocol
import Network.CGI

import Text.ParserCombinators.Parsec

--
-- * Headers
--

class HeaderValue a where
    parseHeaderValue :: Parser a
    prettyHeaderValue :: a -> String

pHeaders :: Parser Headers
pHeaders = many pHeader

pHeader :: Parser (HeaderName, String)
pHeader =
    do name <- many1 headerNameChar
       _ <- char ':'
       _ <- many ws1
       line <- lineString
       _ <- crLf
       extraLines <- many extraFieldLine
       return (HeaderName name, concat (line:extraLines))

extraFieldLine :: Parser String
extraFieldLine =
    do sp <- ws1
       line <- lineString
       _ <- crLf
       return (sp:line)

getHeaderValue :: (Monad m, HeaderValue a) => String -> Headers -> m a
getHeaderValue h hs = lookupM (HeaderName h) hs >>= parseM parseHeaderValue h

--
-- * Parameters (for Content-type etc.)
--

showParameters :: [(String,String)] -> String
showParameters = concatMap f
    where f (n,v) = "; " ++ n ++ "=\"" ++ concatMap esc v ++ "\""
          esc '\\' = "\\\\"
          esc '"'  = "\\\""
          esc c | c `elem` ['\\','"'] = '\\':[c]
                | otherwise = [c]

p_parameter :: Parser (String,String)
p_parameter = try $
  do _ <- lexeme $ char ';'
     p_name <- lexeme $ p_token
     -- Don't allow parameters named q. This is needed for parsing Accept-X
     -- headers. From RFC 2616 14.1:
     --    Note: Use of the "q" parameter name to separate media type
     --    parameters from Accept extension parameters is due to historical
     --    practice. Although this prevents any media type parameter named
     --    "q" from being used with a media range, such an event is believed
     --    to be unlikely given the lack of any "q" parameters in the IANA
     --    media type registry and the rare usage of any media type
     --    parameters in Accept. Future media types are discouraged from
     --    registering any parameter named "q".
     when (p_name == "q") pzero
     _ <- lexeme $ char '='
     -- Workaround for seemingly standardized web browser bug
     -- where nothing is escaped in the filename parameter
     -- of the content-disposition header in multipart/form-data
     let litStr = if p_name == "filename"
                   then buggyLiteralString
                   else literalString
     p_value <- litStr <|> p_token
     return (map toLower p_name, p_value)

instance HeaderValue ContentType where
    parseHeaderValue =
        do _ <- many ws1
           c_type <- p_token
           _ <- char '/'
           c_subtype <- lexeme $ p_token
           c_parameters <- many p_parameter
           return $ ContentType (map toLower c_type) (map toLower c_subtype) c_parameters
    prettyHeaderValue (ContentType x y ps) = x ++ "/" ++ y ++ showParameters ps


getContentType :: Monad m => Headers -> m ContentType
getContentType = getHeaderValue "content-type"

--
-- * Content transfer encoding
--

data ContentTransferEncoding =
	ContentTransferEncoding String
    deriving (Show, Read, Eq, Ord)

instance HeaderValue ContentTransferEncoding where
    parseHeaderValue =
        do _ <- many ws1
           c_cte <- p_token
           return $ ContentTransferEncoding (map toLower c_cte)
    prettyHeaderValue (ContentTransferEncoding s) = s

getContentTransferEncoding :: Monad m => Headers -> m ContentTransferEncoding
getContentTransferEncoding = getHeaderValue "content-transfer-encoding"

--
-- * Content disposition
--

data ContentDisposition =
	ContentDisposition String [(String, String)]
    deriving (Show, Read, Eq, Ord)

instance HeaderValue ContentDisposition where
    parseHeaderValue =
        do _ <- many ws1
           c_cd <- p_token
           c_parameters <- many p_parameter
           return $ ContentDisposition (map toLower c_cd) c_parameters
    prettyHeaderValue (ContentDisposition t hs) =
        t ++ concat ["; " ++ n ++ "=" ++ quote v | (n,v) <- hs]
            where quote x = "\"" ++ x ++ "\"" -- NOTE: silly, but de-facto standard

getContentDisposition :: Monad m => Headers -> m ContentDisposition
getContentDisposition = getHeaderValue "content-disposition"

--
-- * Utilities
--

parseM :: Monad m => Parser a -> SourceName -> String -> m a
parseM p n inp =
  case parse p n inp of
    Left e -> fail (show e)
    Right x -> return x

lookupM :: (Monad m, Eq a, Show a) => a -> [(a,b)] -> m b
lookupM n = maybe (fail ("No such field: " ++ show n)) return . lookup n

caseInsensitiveEq :: String -> String -> Bool
caseInsensitiveEq x y = map toLower x == map toLower y

caseInsensitiveCompare :: String -> String -> Ordering
caseInsensitiveCompare x y = map toLower x `compare` map toLower y

--
-- * Parsing utilities
--

-- | RFC 822 LWSP-char
ws1 :: Parser Char
ws1 = oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; _ <- many ws1; return x

-- | RFC 822 CRLF (but more permissive)
crLf :: Parser String
crLf = try (string "\n\r" <|> string "\r\n") <|> string "\n" <|> string "\r"

-- | One line
lineString :: Parser String
lineString = many (noneOf "\n\r")

literalString :: Parser String
literalString = do _ <- char '\"'
		   str <- many (noneOf "\"\\" <|> quoted_pair)
		   _ <- char '\"'
		   return str

-- No web browsers seem to implement RFC 2046 correctly,
-- since they do not escape double quotes and backslashes
-- in the filename parameter in multipart/form-data.
--
-- Note that this eats everything until the last double quote on the line.
buggyLiteralString :: Parser String
buggyLiteralString =
    do _ <- char '\"'
       str <- manyTill anyChar (try lastQuote)
       return str
  where lastQuote = do _ <- char '\"'
                       notFollowedBy (try (many (noneOf "\"") >> char '\"'))

headerNameChar :: Parser Char
headerNameChar = noneOf "\n\r:"

tspecials, tokenchar :: [Char]
tspecials = "()<>@,;:\\\"/[]?="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ tspecials

p_token :: Parser String
p_token = many1 (oneOf tokenchar)

text_chars :: [Char]
text_chars = map chr ([1..9] ++ [11,12] ++ [14..127])

p_text :: Parser Char
p_text = oneOf text_chars

quoted_pair :: Parser Char
quoted_pair = do _ <- char '\\'
		 p_text