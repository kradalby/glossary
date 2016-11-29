module Glossary exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (type_, checked, name)
import Http
import Json.Decode exposing (Decoder, int, string, list)
import Json.Decode.Pipeline exposing (decode, required)
import List
import List.Extra
import Platform.Cmd exposing (Cmd)
import String
import String.Extra


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


type alias SpecialCharacter =
    { latin : String, special : String }


spanishSpecialCharacters : List SpecialCharacter
spanishSpecialCharacters =
    [ { special = "á", latin = "a" }
    , { special = "é", latin = "e" }
    , { special = "í", latin = "i" }
    , { special = "ó", latin = "o" }
    , { special = "ú", latin = "u" }
    , { special = "ü", latin = "u" }
    , { special = "ñ", latin = "n" }
    ]


type Language
    = English
    | Spanish


type alias Book =
    { title : String
    , chapters : String
    }


type alias Chapter =
    { chapter : Int
    , words : String
    }


type alias Word =
    { english : String
    , spanish : String
    }


type alias Model =
    { wordList : List Word
    , bookList : List Book
    , chapterList : List Chapter
    , currentWord : Word
    , unAnswered : List Word
    , correct : List Word
    , wrong : List Word
    , language : Language
    , textInput : String
    , lazy : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { wordList = words
            , bookList = []
            , chapterList = []
            , currentWord =
                (case List.head words of
                    Nothing ->
                        { english = "", spanish = "" }

                    Just word ->
                        word
                )
            , unAnswered =
                (case List.tail words of
                    Nothing ->
                        []

                    Just words ->
                        words
                )
            , correct = []
            , wrong = []
            , language = English
            , textInput = ""
            , lazy = False
            }
    in
        model ! [ getBooks ]


type Msg
    = Input String
    | Correct
    | Wrong
    | ChangeLanguage Language
    | ToggleLazy
    | GetBooks
    | NewBooks (Result Http.Error (List Book))
    | GetChapters Book
    | NewChapters (Result Http.Error (List Chapter))
    | GetWords Chapter
    | NewWords (Result Http.Error (List Word))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | textInput = newInput }, Cmd.none )

        ChangeLanguage newLanguage ->
            ( { model | language = newLanguage }, Cmd.none )

        ToggleLazy ->
            ( { model | lazy = not model.lazy }, Cmd.none )

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

        GetBooks ->
            ( model, getBooks )

        NewBooks (Ok books) ->
            ( { model | bookList = books }, Cmd.none )

        NewBooks (Err _) ->
            ( model, Cmd.none )

        GetChapters book ->
            ( model, getChapters book )

        NewChapters (Ok chapters) ->
            ( { model | chapterList = chapters }, Cmd.none )

        NewChapters (Err _) ->
            ( model, Cmd.none )

        GetWords chapter ->
            ( model, getWords chapter )

        NewWords (Ok words) ->
            ( { model | wordList = words }, Cmd.none )

        NewWords (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Write the correct translation (Spanish <-> English)" ]
        , h3 [] [ text ("Translate from: " ++ (toString model.language)) ]
        , h4 [] [ text ("Word: " ++ (viewWord model)) ]
        , input [ onInput Input ] []
        , button [ onClick (checkInputWord model) ] [ text "Submit" ]
        , div []
            [ h4 [] [ text "Change language to translate from:" ]
            , viewLanguagePicker
                [ English
                , Spanish
                ]
                model
            , label [] [ input [ type_ "checkbox", onClick (ToggleLazy) ] [] ]
            , text "Lazy"
            ]
        , h4 [] [ text (removeSpecialCharacters model.textInput spanishSpecialCharacters) ]
        ]


viewWord : Model -> String
viewWord model =
    case model.language of
        English ->
            model.currentWord.english

        Spanish ->
            model.currentWord.spanish


viewLanguagePicker : List Language -> Model -> Html Msg
viewLanguagePicker languages model =
    fieldset []
        (List.map
            (\l ->
                radio (toString l) "languagePicker" (l == model.language) (ChangeLanguage l)
            )
            languages
        )


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
                        if (String.toLower model.textInput) == word.spanish then
                            Correct
                        else
                            Wrong

                    Spanish ->
                        if (String.toLower model.textInput) == word.english then
                            Correct
                        else
                            Wrong


radio : String -> String -> Bool -> Msg -> Html Msg
radio labelName groupName isSelected msg =
    label []
        [ input [ type_ "radio", checked isSelected, name groupName, onClick msg ] []
        , text labelName
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Utilities


findWord : String -> List Word -> Maybe Word
findWord input wordList =
    List.Extra.find (\word -> input == word.spanish || input == word.english) wordList


removeSpecialCharacters : String -> List SpecialCharacter -> String
removeSpecialCharacters input list =
    case list of
        [] ->
            input

        first :: rest ->
            removeSpecialCharacters (String.Extra.replace first.special first.latin input) rest



-- Api stuff


base_url : String
base_url =
    "/api"


getBooks : Cmd Msg
getBooks =
    let
        url =
            base_url ++ "/book"

        request =
            Http.get url (list bookDecoder)
    in
        Http.send NewBooks request


bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "title" string
        |> required "chapters" string


getChapters : Book -> Cmd Msg
getChapters book =
    let
        url =
            base_url ++ book.chapters

        request =
            Http.get url (list chapterDecoder)
    in
        Http.send NewChapters request


chapterDecoder : Decoder Chapter
chapterDecoder =
    decode Chapter
        |> required "chapter" int
        |> required "words" string


getWords : Chapter -> Cmd Msg
getWords chapter =
    let
        url =
            base_url ++ chapter.words

        request =
            Http.get url (list wordDecoder)
    in
        Http.send NewWords request


wordDecoder : Decoder Word
wordDecoder =
    decode Word
        |> required "english" string
        |> required "spanish" string
