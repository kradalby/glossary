module Glossary exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import List
import List.Extra
import Platform.Cmd exposing (Cmd)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


words : List Word
words =
    [ { english = "speak", spanish = "hablar" }, { english = "eat", spanish = "comer" } ]


type Language
    = English
    | Spanish


type alias Word =
    { english : String
    , spanish : String
    }


type alias Model =
    { wordList : List Word
    , currentWord : Word
    , unAnswered : List Word
    , correct : List Word
    , wrong : List Word
    , language : Language
    , textInput : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (case List.tail words of
            Nothing ->
                []

            Just words ->
                words
        )
        (case List.head words of
            Nothing ->
                { english = "", spanish = "" }

            Just word ->
                word
        )
        []
        []
        English
        ""
    , Cmd.none
    )


type Msg
    = Input String
    | Correct
    | Wrong


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | textInput = newInput }, Cmd.none )

        Correct ->
            ( { model
                | currentWord =
                    case List.head model.unAnswered of
                        Nothing ->
                            { english = "", spanish = "" }

                        Just head ->
                            head
                , unAnswered =
                    case List.tail model.unAnswered of
                        Nothing ->
                            []

                        Just tail ->
                            tail
                , correct = model.currentWord :: model.correct
              }
            , Cmd.none
            )

        Wrong ->
            ( { model
                | currentWord =
                    case List.head model.unAnswered of
                        Nothing ->
                            { english = "", spanish = "" }

                        Just head ->
                            head
                , unAnswered =
                    case List.tail model.unAnswered of
                        Nothing ->
                            []

                        Just tail ->
                            tail
                , wrong = model.currentWord :: model.wrong
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Write the correct translation (Spanish <-> English)" ]
        , h3 [] [ text ("Translate from: " ++ (toString model.language)) ]
        , h4 [] [ text ("Word: " ++ model.currentWord.english) ]
        , input [ onInput Input ] []
        , button [ onClick (checkInputWord model) ] [ text "Submit" ]
        , h4 [] [ text model.textInput ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


findWord : String -> List Word -> Maybe Word
findWord input wordList =
    List.Extra.find (\word -> input == word.spanish || input == word.english) wordList


checkInputWord : Model -> Msg
checkInputWord model =
    let
        word =
            findWord model.textInput model.wordList
    in
        case word of
            Nothing ->
                Wrong

            Just word ->
                case model.language of
                    English ->
                        if model.textInput == word.spanish then
                            Correct
                        else
                            Wrong

                    Spanish ->
                        if model.textInput == word.english then
                            Correct
                        else
                            Wrong
