module Bingo exposing (..)

import Browser
import Html exposing (Html, div, input, span, table, text, th, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.List as Random
import Set.Any as AnySet exposing (AnySet)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Space
    = Number Int
    | Free


type alias SpaceSet =
    AnySet ( Int, Int ) Space


type B
    = B Space Space Space Space Space


type I
    = I Space Space Space Space Space


type N
    = N Space Space Space Space Space


type G
    = G Space Space Space Space Space


type O
    = O Space Space Space Space Space


type Board
    = Board B I N G O


type alias Model =
    { game : String, username : String, board : Board, marked : SpaceSet }


getShuffledNumbers : Int -> Int -> Random.Generator (List Space)
getShuffledNumbers lower upper =
    List.range lower upper |> List.map Number |> Random.shuffle


getColumn letter lower upper =
    Random.map (take5 letter) (getShuffledNumbers lower upper)
        |> Random.map (Maybe.withDefault (letter (Number 1) (Number 2) (Number 3) (Number 4) (Number 5)))


getB : Random.Generator B
getB =
    getColumn B 1 15


getI =
    getColumn I 16 30


getN =
    getColumn (\a b _ c d -> N a b Free c d) 31 45


getG =
    getColumn G 46 60


getO =
    getColumn O 61 75


take5 : (Space -> Space -> Space -> Space -> Space -> a) -> List Space -> Maybe a
take5 constructor values =
    case values of
        a :: b :: c :: d :: e :: _ ->
            constructor a b c d e |> Just

        _ ->
            Nothing


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = "", username = "", board = basicBoard, marked = AnySet.empty compareSpace }
    , Cmd.none
    )


compareSpace space =
    case space of
        Number number ->
            ( 0, number )

        Free ->
            ( 1, 0 )


basicBoard : Board
basicBoard =
    Board
        (B (Number 1) (Number 2) (Number 3) (Number 4) (Number 5))
        (I (Number 16) (Number 17) (Number 18) (Number 19) (Number 20))
        (N (Number 31) (Number 32) Free (Number 34) (Number 35))
        (G (Number 46) (Number 47) (Number 48) (Number 49) (Number 50))
        (O (Number 61) (Number 62) (Number 63) (Number 64) (Number 65))



-- UPDATE


type Msg
    = SetName String
    | SetGame String
    | MarkSpace Space
    | MakeBoard Int


update msg model =
    case msg of
        SetName name ->
            ( { model | username = name }, Cmd.none )

        SetGame game ->
            ( { model | game = game }, Cmd.none )

        MarkSpace space ->
            let
                _ =
                    Debug.log "Space:" space
            in
            ( { model | marked = AnySet.insert space model.marked }, Cmd.none )

        -- TODO: Use random value
        MakeBoard seed ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
                    []
                    [ div [] [ span [ class "inputLabel" ] [ text "Game: " ], input [ class "inputField", placeholder "ID of the game", value model.game, onInput SetGame ] [] ]
                    , div [] [ span [ class "inputLabel" ] [ text "Username: " ], input [ class "inputField", placeholder "Your Twitch username", value model.username, onInput SetName ] [] ]
                    , viewBoard model.marked model.board
                    ]
                ]


viewBoard : SpaceSet -> Board -> Html Msg
viewBoard marked (Board (B b1 b2 b3 b4 b5) (I i1 i2 i3 i4 i5) (N n1 n2 n3 n4 n5) (G g1 g2 g3 g4 g5) (O o1 o2 o3 o4 o5)) =
    let
        spaceSpan =
            spaceToSpan marked
    in
    table []
        [ th [ class "boardHeader" ] (List.map (text >> List.singleton >> span [ class "boardSpace" ]) [ "B", "I", "N", "G", "O" ])
        , tr [ class "boardRow" ] (List.map spaceSpan [ b1, i1, n1, g1, o1 ])
        , tr [ class "boardRow" ] (List.map spaceSpan [ b2, i2, n2, g2, o2 ])
        , tr [ class "boardRow" ] (List.map spaceSpan [ b3, i3, n3, g3, o3 ])
        , tr [ class "boardRow" ] (List.map spaceSpan [ b4, i4, n4, g4, o4 ])
        , tr [ class "boardRow" ] (List.map spaceSpan [ b5, i5, n5, g5, o5 ])
        ]


spaceToSpan : SpaceSet -> Space -> Html Msg
spaceToSpan marked s =
    let
        isMarked =
            if AnySet.member s marked then
                [ class "markedSpace" ]

            else
                []

        attributes =
            List.append [ class "boardSpace", onClick (MarkSpace s) ] isMarked
    in
    s |> spaceToText |> text |> List.singleton |> span attributes


spaceToText space =
    case space of
        Number n ->
            String.fromInt n

        Free ->
            "Free!"
