module TangoScript.DoubleQuoteString exposing(doubleQuoteString)

import Char
import Parser exposing (..)


doubleQuoteString : Parser String
doubleQuoteString =
  succeed identity
    |. token "\""
    |= loop (0, []) stringHelp

stringHelp : (Int, List String) -> Parser (Step (Int, List String) String)
stringHelp (offset, revChunks) =
    let
        stringP : Parser (Step (List String) String)
        stringP =
            oneOf [ succeed (\chunk -> Loop (chunk :: revChunks))
                      |. token "\\"
                      |= oneOf
                        [ map (\_ -> "\n") (token "n")
                        , map (\_ -> "\t") (token "t")
                        , map (\_ -> "\r") (token "r")
                        , succeed String.fromChar
                            |. token "u{"
                            |= unicode
                            |. token "}"
                        ]
                  , token "\""
                      |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
                  , chompWhile isUninteresting
                      |> getChompedString
                      |> map (\chunk -> Loop (chunk :: revChunks))
                  ]

    in
        succeed (\s o -> (o, s))
            |= stringP
            |= getOffset
            |> andThen (\(newOffset, step) ->
                            case step of
                                Done state -> succeed (Done state)
                                Loop state ->
                                    case newOffset == offset of
                                        True -> problem "orphan double quoted string"
                                        False -> succeed (Loop (newOffset, state))
                       )


isUninteresting : Char -> Bool
isUninteresting char =
  char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
  getChompedString (chompWhile Char.isHexDigit)
    |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
  let
    length = String.length str
    code = String.foldl addHex 0 str
  in
  if 4 <= length && length <= 6 then
    problem "code point must have between 4 and 6 digits"
  else if 0 <= code && code <= 0x10FFFF then
    succeed (Char.fromCode code)
  else
    problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
  let
    code = Char.toCode char
  in
  if 0x30 <= code && code <= 0x39 then
    16 * total + (code - 0x30)
  else if 0x41 <= code && code <= 0x46 then
    16 * total + (10 + code - 0x41)
  else
    16 * total + (10 + code - 0x61)
