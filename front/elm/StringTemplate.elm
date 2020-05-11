module StringTemplate exposing ( stringTemplateToString
                               , stringToTemplate
                               , stringTemplateParser
                               , templateToString
                               )

import Application.Type exposing (..)
import Parser as P exposing((|.), (|=), Step, Parser)
import Set
import Result


stringTemplateToString : StringTemplate -> String
stringTemplateToString stringTemplate =
    stringTemplate |> List.map templateToString |> String.join ""

templateToString : Template -> String
templateToString template =
    case template of
        Sentence s -> s
        Key s -> "{{" ++ s ++ "}}"

stringToTemplate : String -> StringTemplate
stringToTemplate str =
    case P.run stringTemplateParser str of
        Ok stringTemplate -> stringTemplate
        _ -> [ Sentence str ]

stringTemplateParser : Parser StringTemplate
stringTemplateParser =
    P.loop ([], 0) (ifProgress templateParser)

ifProgress : Parser a -> (List a, Int) -> Parser (Step (List a, Int) (List a))
ifProgress parser (acc, offset) =
    let
        loop : a -> Int -> Step (List a, Int) (List a)
        loop template newOffset =
            case offset == newOffset of
                True -> P.Done (List.reverse (template :: acc))
                False -> P.Loop (template :: acc, newOffset)

    in
        parser |> P.andThen (\template -> P.getOffset |> P.map (\newOffset -> loop template newOffset))


templateParser : Parser Template
templateParser =
    P.succeed identity
        |= P.oneOf
           [ P.backtrackable keyParser
           , sentenceParser
           ]

keyParser : Parser Template
keyParser =
    P.succeed Key
    |. P.symbol "{{"
    |. P.spaces
    |= P.variable
       { start = always True
       , inner = \c -> Char.isAlphaNum c || List.member c validCharsForKey
       , reserved = Set.fromList ["{{", "}}"]
       }
    |. P.spaces
    |. P.symbol "}}"

validCharsForKey : List Char
validCharsForKey =
    String.toList "&/-,.|~:#%'=[]()<>`!_@*$"

sentenceParser : Parser Template
sentenceParser =
    P.succeed Sentence
        |= P.getChompedString (P.chompUntilEndOr "{{")
