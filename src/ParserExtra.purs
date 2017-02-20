module ParserExtra (regex) where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, length)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (startsWith)
import Prelude ((<>), (+), ($), show)
import Text.Parsing.StringParser (Parser(..), ParseError(..), fail)
import Data.Array (uncons)

regex :: String -> Parser String
regex pat =
  let
    pattern =
        if startsWith "^" pat then
            pat
        else
            "^" <> pat
    er = Regex.regex pattern noFlags
  in
    case er of
      Left _ ->
        fail $ "Illegal regex " <> show pat
      Right r ->
        Parser \{ str, pos } ->
          let
            remainder = drop pos str
          in
            -- reduce the possible array of matches to 0 or 1 elements to aid Array pattern matching
            case uncons $ fromMaybe [] $ Regex.match r remainder of
              Just { head: Just matched, tail: _ }  ->
                  Right { result: matched, suffix: { str, pos: pos + length matched } }
              _ ->
                  Left { pos, error: ParseError $ "Regex pattern " <> show pat <> " did not match" }
