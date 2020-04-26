module Player exposing (..)

import Bingo exposing (Board, Msg(..), Space, SpaceSet, compareSpace, makeBoard, newBoard, viewBoard)
import Browser
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onInput)
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
    { game : String, username : String, board : Board, marked : SpaceSet, admin : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = "", username = "", board = makeBoard "", marked = AnySet.empty compareSpace, admin = False }
    , Cmd.none
    )


update msg model =
    case msg of
        SetName name ->
            { model | username = name } |> newBoard

        SetGame game ->
            { model | game = game } |> newBoard

        MarkSpace space ->
            ( { model | marked = AnySet.toggle space model.marked }, Cmd.none )



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
                    [ div [] [ span [ class "inputLabel" ] [ text "Game ID:" ], input [ class "inputField", placeholder "ID of the game", value model.game, onInput SetGame ] [] ]
                    , div [] [ span [ class "inputLabel" ] [ text "Username:" ], input [ class "inputField", placeholder "Your Twitch username", value model.username, onInput SetName ] [] ]
                    , viewBoard model.marked model.board
                    ]
                ]
