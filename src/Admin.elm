module Admin exposing (..)

import Bingo exposing (Board, Msg(..), Space(..), SpaceSet, compareSpace, makeBoard, newBoard, viewBoard)
import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Set.Any as AnySet



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { game : String, username : String, board : Board, marked : SpaceSet, admin : Bool, numberInput : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = "", username = "", board = makeBoard "", marked = AnySet.empty compareSpace |> AnySet.insert Free, admin = True, numberInput = "" }
    , Cmd.none
    )


update msg model =
    case msg of
        SetName name ->
            { model | username = name } |> newBoard

        SetGame game ->
            { model | game = game } |> newBoard

        MarkSpace space ->
            ( { model | marked = AnySet.toggle space model.marked, numberInput = "" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        _ ->
            div []
                [ Html.node "link"
                    [ Html.Attributes.rel "stylesheet", Html.Attributes.href "bingo.css" ]
                    []
                , div
                    [ class "everything" ]
                    [ viewAnnouncements model
                    , div [] [ span [ class "inputLabel" ] [ text "Game ID:" ], input [ class "inputField", placeholder "ID of the game", value model.game, onInput SetGame ] [] ]
                    , div [] [ span [ class "inputLabel" ] [ text "Username:" ], input [ class "inputField", placeholder "Twitch username of the caller", value model.username, onInput SetName ] [] ]
                    , viewBoard model.marked model.board
                    ]
                ]


onEnter tagger =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed "Enter pressed"

            else
                Json.Decode.fail "is not enter - is this error shown anywhere?!"

        decode_Enter =
            Json.Decode.andThen isEnter Html.Events.keyCode
    in
    Html.Events.on "keydown" (Json.Decode.map2 (\_ value -> tagger value) decode_Enter Html.Events.targetValue)


getNumber : String -> Msg
getNumber string =
    String.toInt string |> Maybe.map Number |> Maybe.withDefault Free |> MarkSpace


viewAnnouncements : Model -> Html Msg
viewAnnouncements model =
    div [] (List.append [ span [ class "numberInputText" ] [ text "Enter numbers:" ], input [ class "numberInput", onEnter getNumber, value model.numberInput ] [] ] (AnySet.toList model.marked |> List.map viewSpace))


viewSpace : Space -> Html Msg
viewSpace space =
    case space of
        Number number ->
            button [ class "spaceButton", onClick (MarkSpace space) ] [ text <| String.fromInt number ]

        _ ->
            span [] []
