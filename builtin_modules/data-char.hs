 {- data-char.hs : Data.Char module learning
 - rewrite and use functions found in Data.Char
 - way to keep a record of what I know
 -}

{- May have functions that clash with Prelude
 - so need to qualify
 -}

import Data.Char
import Data.Function (on)
import Data.List (groupBy)

{- CHARACTER PREDICATES
 -
 - The following are all of the predicates over characters in Data.Char
 -
 - isControl : checks whether character is a control character
 -
 - isSpace : checks whether character is a white-space related character
 -           (space, tab, newline, etc)
 -
 - isLower : checks whether character is lowercase
 -
 - isUpper : checks whether character is uppercase
 -
 - isAlpha : checks whether character is a letter
 - 
 - isAlphaNum : checks whether character is a letter or number
 -
 - isPrint : checks whether character is printable
 -
 - isDigit : checks whether character is a digit
 -
 - isOctDigit : checks whether character is an octal digit
 -
 - isHexDigit : checks whether character is a hex digit
 -
 - isLetter : checks whether character is a letter
 -
 - isMark : checks for unicode mark characters (accented letters)
 -
 - isNumber : checks whether character is numeric
 -
 - isPunctuation : checks whether character is punctuation
 -
 - isSymbol : checks whether character is a math/curreny symbol
 -
 - isSeperator : checks for unicode spaces and seperators
 -
 - isAscii : checks whether character is in ASCII (unicode 0-127)
 -
 - isLatin1 : checks whether character is in unicode 0-255
 -
 - isAsciiUpper : checks whether character is ASCII and uppercase
 -
 - isAsciiLower : checks whether character is ASCII and lowecase
 -}

--simulate words using groupBy, on, and isSpace
words' :: String -> [String]
words' = 
  filter (not . any isSpace) . groupBy ((==) `on` isSpace)


{- Data.Char has a datatype called GeneralCategory, that gives more
 - specific type information about a Char.
 - Some examples of types are:
 -  Space
 -  Control
 -  UppercaseLetter
 -  DecimalNUmber
 -  OtherPunctuation
 -  MathSymbol
 -}


{- CHARACTER CONVERSIONS
 -
 - toUpper : converts to uppercase if possible
 -
 - toLower : converts to lowercase if possible
 -
 - toTitle : converts to titlecase (uppercase for most characters) if possible
 -
 - digitToInt : converts character in ranges of '0'..'9', 'a'..'f','A'..'F'
 - to their integer equivalent
 -
 - intToDigit : converts integer in range of 0..15 to corresponding lowercase
 - hex character
 -
 - ord : converts character to its unicode value
 -
 - chr : converts int to its character in unicode
 -}

--Caesar cipher (enocding by offsetting each letter by a set amount)
cipherEncode :: Int -> String -> String
cipherEncode shift msg = map (chr . (+ shift) . ord) msg

cipherDecode :: Int -> String -> String
cipherDecode shift msg = cipherEncode (negate shift) msg


