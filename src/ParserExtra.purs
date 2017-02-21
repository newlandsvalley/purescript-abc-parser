module ParserExtra (regex, regex') where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, length)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (startsWith)
import Prelude ((<>), (+), ($))
import Text.Parsing.StringParser (Parser(..), ParseError(..), fail)
import Data.Array (uncons)

--| Build the regular expression from the pattern and match it
regex' :: String -> Parser String
regex' pat =
    case er of
      Left _ ->
        fail $ "Illegal regex " <> pat
      Right r ->
        regex r
    where
      pattern =
        if startsWith "^" pat then
          pat
        else
          "^" <> pat
      er = Regex.regex pattern noFlags

-- | Match the regular expression
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
          if startsWith matched remainder then
            Right { result: matched, suffix: { str, pos: pos + length matched } }
          else
            Left { pos, error: ParseError $ "no match - consider prefacing the pattern with '^'" }
        _ ->
            Left { pos, error: ParseError $ "no match" }
