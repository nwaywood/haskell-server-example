module Main exposing (..)

import Browser
import Button exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Font exposing (bold, size)
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, int, list, map4, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { topics : Status (List String)
    , currentTopic : SelectedTopic String
    , comments : CommentStatus (List Comment)
    , newTopic : Maybe String
    , newComment : Maybe String
    , submitStatus : SubmitStatus
    }


type SelectedTopic a
    = NoTopic
    | Topic a


type SubmitStatus
    = FailSubmit
    | NoSubmit
    | SucceedSubmit
    | LoadingSubmit


type alias Comment =
    { id : Int
    , topic : String
    , text : String
    , time : String
    }


type CommentStatus s
    = LoadingC
    | NotLoadedC
    | FailureC
    | SuccessC s


type Status a
    = Loading
    | Failure
    | Success a


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics = Loading
      , comments = NotLoadedC
      , currentTopic = NoTopic
      , newTopic = Nothing
      , newComment = Nothing
      , submitStatus = NoSubmit
      }
    , getTopics
    )



-- UPDATE


type Msg
    = ReloadTopics
    | OnChangeTopic String
    | OnChangeComment String
    | OnSubmitComment
    | SelectTopic String
    | GotTopics (Result Http.Error (List String))
    | GotComments (Result Http.Error (List Comment))
    | GotSubmitResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReloadTopics ->
            ( { model | topics = Loading }, getTopics )

        OnChangeTopic t ->
            ( { model
                | newTopic =
                    if t == "" then
                        Nothing

                    else
                        Just t
              }
            , Cmd.none
            )

        OnChangeComment t ->
            ( { model
                | newComment =
                    if t == "" then
                        Nothing

                    else
                        Just t
              }
            , Cmd.none
            )

        OnSubmitComment ->
            case ( model.newTopic, model.newComment ) of
                ( Just t, Just c ) ->
                    ( { model | submitStatus = LoadingSubmit }, submitComment t c )

                _ ->
                    ( model, Cmd.none )

        SelectTopic t ->
            case model.currentTopic of
                Topic oldTopic ->
                    if t == oldTopic then
                        ( model, Cmd.none )

                    else
                        ( { model | currentTopic = Topic t }, getComments t )

                NoTopic ->
                    ( { model | currentTopic = Topic t }, getComments t )

        GotTopics result ->
            case result of
                Ok ts ->
                    ( { model | topics = Success ts }, Cmd.none )

                Err _ ->
                    ( { model | topics = Failure }, Cmd.none )

        GotComments result ->
            case result of
                Ok cs ->
                    ( { model | comments = SuccessC cs }, Cmd.none )

                Err _ ->
                    ( { model | comments = FailureC }, Cmd.none )

        GotSubmitResult result ->
            case result of
                Ok s ->
                    ( { model | submitStatus = SucceedSubmit, newTopic = Nothing, newComment = Nothing }, getTopics )

                Err _ ->
                    ( { model | submitStatus = FailSubmit }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ padding 12, spacing 12, width fill ]
            [ el [ size 24 ] <| text "Amazing Reddit Clone"
            , viewCommentSubmit model
            , button [ padding 12 ]
                { onPress = Just ReloadTopics
                , label = text "Refresh Topics"
                }
            , row [ width fill, spaceEvenly ]
                [ el [ width fill, alignTop ] (viewTopics model)
                , el [ width fill, alignTop ] (viewComments model.comments)
                ]
            ]


viewCommentSubmit : Model -> Element Msg
viewCommentSubmit model =
    row [ spacing 12 ]
        [ Input.text []
            { onChange = OnChangeTopic
            , text =
                case model.newTopic of
                    Just s ->
                        s

                    Nothing ->
                        ""
            , label = Input.labelAbove [] (text "Topic")
            , placeholder = Just (Input.placeholder [] (text "New Topic"))
            }
        , Input.text []
            { onChange = OnChangeComment
            , text =
                case model.newComment of
                    Just s ->
                        s

                    Nothing ->
                        ""
            , label = Input.labelAbove [] (text "Comment")
            , placeholder = Just (Input.placeholder [] (text "New Comment"))
            }
        , button [ centerY, padding 12 ] { onPress = Just OnSubmitComment, label = text "Submit" }
        , viewSubmitStatus model
        ]


viewSubmitStatus : Model -> Element Msg
viewSubmitStatus model =
    case model.submitStatus of
        FailSubmit ->
            text "Failed to post new comment"

        LoadingSubmit ->
            text "Submitting..."

        SucceedSubmit ->
            text "Successfully submitted new comment"

        NoSubmit ->
            text ""


viewTopics : Model -> Element Msg
viewTopics model =
    let
        topicEl t =
            Input.button
                (case model.currentTopic of
                    Topic top ->
                        if top == t then
                            [ bold ]

                        else
                            []

                    NoTopic ->
                        []
                )
                { onPress = Just (SelectTopic t), label = text t }
    in
    case model.topics of
        Failure ->
            el [] <| text "Failed to load topics"

        Loading ->
            text "Loading..."

        Success [] ->
            text "No topics available"

        Success ts ->
            column [ spacing 12 ] (List.map topicEl ts)


viewComments : CommentStatus (List Comment) -> Element Msg
viewComments comments =
    case comments of
        FailureC ->
            el [] <| text "Failed to load comments"

        LoadingC ->
            text "Loading..."

        NotLoadedC ->
            text ""

        SuccessC [] ->
            text "No comments available"

        SuccessC cs ->
            column [ spacing 12, width fill ] (List.map viewComment cs)


viewComment : Comment -> Element Msg
viewComment c =
    column [ Border.widthEach { edges | bottom = 1 }, width fill, paddingEach { edges | bottom = 12 } ]
        [ el [] <| text c.text
        , row [ spacing 12 ]
            [ text <| String.fromInt c.id
            , text c.time
            ]
        ]


edges =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 0
    }



-- HTTP


getTopics : Cmd Msg
getTopics =
    Http.get
        { url = "/api/list"
        , expect = Http.expectJson GotTopics topicDecoder
        }


getComments : String -> Cmd Msg
getComments t =
    Http.get
        { url = "/api/" ++ t ++ "/view"
        , expect = Http.expectJson GotComments commentsDecoder
        }


submitComment : String -> String -> Cmd Msg
submitComment topic comment =
    Http.post
        { url = "/api/" ++ topic ++ "/add"
        , body = Http.stringBody "text/plain" comment
        , expect = Http.expectString GotSubmitResult
        }



-- JSON decoders


topicDecoder : Decoder (List String)
topicDecoder =
    Json.Decode.list string


commentsDecoder : Decoder (List Comment)
commentsDecoder =
    Json.Decode.list commentDecoder


commentDecoder : Decoder Comment
commentDecoder =
    map4 Comment
        (field "id" int)
        (field "topic" string)
        (field "text" string)
        (field "time" string)
