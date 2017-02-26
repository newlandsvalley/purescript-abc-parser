module ParserExtra (regex, regex') where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), drop, length, stripPrefix)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Prelude ((<>), (+), ($))
import Text.Parsing.StringParser (Parser(..), ParseError(..), fail)
import Data.Array (uncons)

-- | Build the regular expression from the pattern and match it, ensuring
-- | that the pattern only attempts to match from the start of the target.
regex' :: String -> Parser String
regex' pat =
    case Regex.regex pattern noFlags of
      Left _ ->
        fail $ "Text.Parsing.StringParser.String.regex': illegal regex " <> pat
      Right r ->
        regex r
    where
      pattern =
        case stripPrefix (Pattern "^") pat of
          Nothing ->
            "^" <> pat
          _ ->
            pat

-- | Match the regular expression.
regex :: Regex.Regex -> Parser String
regex r =
  Parser \{ str, pos } ->
    let
      remainder = drop pos str
    in
      -- reduce the possible array of matches to 0 or 1 elements to aid Array pattern matching
      case uncons $ fromMaybe [] $ Regex.match r remainder of
        Just { head: Just matched, tail: _ }  ->
          -- only accept matches at position 0
          case stripPrefix (Pattern matched) remainder of
            Nothing ->
              Left { pos, error: ParseError $ "Text.Parsing.StringParser.String.regex: no match - consider prefacing the pattern with '^'" }
            _ ->
              Right { result: matched, suffix: { str, pos: pos + length matched } }
        _ ->
          Left { pos, error: ParseError $ "no match" }
