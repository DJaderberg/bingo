module Bingo exposing (..)

import Html exposing (Html, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Murmur3 exposing (hashString)
import Random
import Random.List as Random
import Set.Any as AnySet exposing (AnySet)



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


getBoard : Random.Generator Board
getBoard =
    Random.map5 Board getB getI getN getG getO


take5 : (Space -> Space -> Space -> Space -> Space -> a) -> List Space -> Maybe a
take5 constructor values =
    case values of
        a :: b :: c :: d :: e :: _ ->
            constructor a b c d e |> Just

        _ ->
            Nothing


compareSpace space =
    case space of
        Number number ->
            ( 0, number )

        Free ->
            ( 1, 0 )



-- UPDATE


type Msg
    = SetName String
    | SetGame String
    | MarkSpace Space


newBoard model =
    let
        board =
            makeBoard <| String.toLower model.game ++ String.toLower model.username
    in
    ( { model | board = board }, Cmd.none )


makeBoard : String -> Board
makeBoard seed =
    Random.step getBoard (Random.initialSeed (hashString 42069 <| seed)) |> Tuple.first


viewBoard : SpaceSet -> Board -> Html Msg
viewBoard marked (Board (B b1 b2 b3 b4 b5) (I i1 i2 i3 i4 i5) (N n1 n2 n3 n4 n5) (G g1 g2 g3 g4 g5) (O o1 o2 o3 o4 o5)) =
    let
        spaceSpan =
            spaceToTd marked
    in
    table [ class "board" ]
        [ thead [] [ th [ class "boardHeader" ] (List.map (text >> List.singleton >> span [ class "boardSpace" ]) [ "B", "I", "N", "G", "O" ]) ]
        , tbody []
            [ tr [ class "boardRow" ] (List.map spaceSpan [ b1, i1, n1, g1, o1 ])
            , tr [ class "boardRow" ] (List.map spaceSpan [ b2, i2, n2, g2, o2 ])
            , tr [ class "boardRow" ] (List.map spaceSpan [ b3, i3, n3, g3, o3 ])
            , tr [ class "boardRow" ] (List.map spaceSpan [ b4, i4, n4, g4, o4 ])
            , tr [ class "boardRow" ] (List.map spaceSpan [ b5, i5, n5, g5, o5 ])
            ]
        ]


spaceToTd : SpaceSet -> Space -> Html Msg
spaceToTd marked s =
    let
        isMarked =
            if AnySet.member s marked then
                [ class "markedSpace" ]

            else
                []

        attributes =
            List.append [ class "boardSpace", onClick (MarkSpace s) ] isMarked
    in
    s |> spaceToText |> text |> List.singleton |> td attributes


spaceToText space =
    case space of
        Number n ->
            String.fromInt n

        Free ->
            "Free"
