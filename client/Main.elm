module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html exposing (program)
import Html.Events exposing (..)
import Http
import Api exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { items : Dict Int Item
    , addItemInput : String
    , error : Maybe String
    , station : Cmd Msg
    , result : Maybe Index
    }


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        fetch =
            Http.send (fromServer Initial) Api.getApiItem

        getStation =
            Http.send (fromServer NewStation) Api.getApiTabl

        state =
            { items = empty, addItemInput = "", error = Nothing, station = getStation, result = Nothing }
    in
        ( state, fetch )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId
    | NewStation Station


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId
    | ShowStation1


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( s
                    , itemIds
                        |> List.map getApiItemByItemId
                        |> List.map (Http.send (fromServer NewItem))
                        |> Cmd.batch
                    )

                NewItem item ->
                    { s | items = insert item.id item s.items } ! []

                Delete id ->
                    { s | items = remove id s.items } ! []

                NewStation station ->
                    { s | result = Just (station.no2IndexLevel) } ! []

        FromUi fromUi ->
            case fromUi of
                ShowStation1 ->
                    let
                        cmd =
                            Http.send (fromServer NewStation) Api.getApiTabl
                    in
                        ( s , cmd)

                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            --Http.send (fromServer (\id -> NewItem (Item id new))) (postApiItem new)
                            Http.send (fromServer NewStation) Api.getApiTabl

                        newState =
                            { s | addItemInput = "" }
                    in
                        if new == "" then
                            update (Error "empty field") s
                        else
                            ( newState, cmd )

                AddItemInputChange t ->
                    { s | addItemInput = t } ! []

                Done id ->
                    ( s, Http.send (fromServer (\NoContent -> Delete id)) (deleteApiItemByItemId id) )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| toString error



-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ text (toString state)
        , br [] []
        ]
            ++ List.map (viewItem << Tuple.second) (toList state.items)
            ++ [ input [ onInput (FromUi << AddItemInputChange) ] []
               , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
               ]
            ++ [ table []
                    [ thead []
                          [ tr []
                              [ th [ ] [ text "xd" ]
                              , th [ ] [ text "Rank" ]
                              , th [ ] [ text "Pollution" ]
                              ]
                          ]
                      , tbody []
                          [ tr []
                              [ td [] [ text "Krakow" ]
                              , td [ ] [ text "1" ]
                              , td [ ] [ text "156,34 mg" ]
                              ]
                              ,
                              tr []
                               [ td [] [ text "Tarnow" ]
                               , td [ ] [ text "2" ]
                               , td [ ] [ text "109,84 mg" ]
                               ]
                          ]
                      ]
               ]
            ++ [ button [ onClick (FromUi ShowStation1)] [ text "Station 1"]
               , button [ onClick (FromUi ShowStation1)] [ text "Station 2"]
               , button [ onClick (FromUi ShowStation1)] [ text "Station 3"]
               ]
            ++ [ text (toString (state))
               , br [] []
               ]


viewItem : Item -> Html Msg
viewItem item =
    div [] <|
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]
