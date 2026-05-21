module IrcParsing.Parsing (Message (..), parseMessage) where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.List (intercalate)
import IrcParsing.Types (Message, Prefix)
import qualified IrcParsing.Types as Types
import Text.Parsec
import Text.Parsec.String (Parser)

-- https://www.rfc-editor.org/rfc/rfc2812#section-2.3.1

message :: Parser Message
message = do
  messagePrefix <- prefix
  messageCommand <- command
  Types.Message messagePrefix messageCommand <$> params

prefix :: Parser Prefix
prefix =
  option (Types.UserName "" "" "") $
    char ':' *> (username <|> servername) <* space

command :: Parser String
command = many1 letter <|> count 3 digit

params :: Parser [String]
params = try fourteenOrLess <|> fourteen
  where
    singletonIfnotNull [] = []
    singletonIfnotNull x = [x]
    fourteenOrLess =
      (++)
        <$> option [] (choice [try $ count i (space *> middle) | i <- [1 .. 14]])
        <*> (singletonIfnotNull <$> option [] (space *> char ':' *> trailing))
    fourteen =
      (++)
        <$> count 14 (space *> middle)
        <*> (singletonIfnotNull <$> option [] (space *> optional (char ':') *> trailing))

nospcrlfcl :: Parser Char
nospcrlfcl = noneOf ['\0', '\r', '\n', ' ', ':']

middle :: Parser String
middle =
  (:)
    <$> nospcrlfcl
    <*> many (char ':' <|> nospcrlfcl)

trailing :: Parser String
trailing = many (char ':' <|> char ' ' <|> nospcrlfcl)

username :: Parser Prefix
username = try $ Types.UserName <$> nickname <*> (char '!' *> user) <*> (char '@' *> host)

servername :: Parser Prefix
servername = Types.ServerName <$> hostname

host :: Parser String
host = hostname <|> hostaddr

hostname :: Parser String
hostname = intercalate "." <$> sepBy1 shortname (char '.')

shortname :: Parser String
shortname =
  (:)
    <$> (letter <|> digit)
    <*> ( (++)
            <$> many (letter <|> digit <|> char '-')
            <*> many (letter <|> digit)
        )

hostaddr :: Parser String
hostaddr = ip4addr <|> ip6addr

ip4addr :: Parser String
ip4addr = (++) <$> ip4Part <*> (concat <$> count 3 ((:) <$> char '.' <*> ip4Part))
  where
    ip4Part = choice [try $ count i digit | i <- [1 .. 3]]

ip6addr :: Parser String
ip6addr =
  try (intercalate ":" <$> sepBy1 (many1 hexDigit) (char ':'))
    <|> ( (++)
            <$> string "0:0:0:0:0:"
            <*> ((++) <$> (string "0" <|> string "FFFF") <*> ip4addr)
        )

nickname :: Parser String
nickname =
  (:)
    <$> (letter <|> special)
    <*> many (choice [letter, digit, special, char '-'])

user :: Parser String
user = many1 (noneOf ['\0', '\r', '\n', ' ', '@'])

special :: Parser Char
special = oneOf "[]\\`_^{|}"

parseMessage :: ByteString -> Message
parseMessage msg = case parse message "" msgS of
  Left err -> Types.Message (Types.UserName "TinIRC client" "" "") msgS ["- Raw", show err]
  Right res -> res
  where
    msgS = toString msg
