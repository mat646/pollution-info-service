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
    , result : Maybe Station
    }


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        fetch =
            Http.send (fromServer Initial) Api.getApiItem

        state =
            { items = empty, addItemInput = "", error = Nothing, result = Nothing }
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
    | ShowStation2


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
                    { s | result = Just (station) } ! []

        FromUi fromUi ->
            case fromUi of
                ShowStation1 ->
                    let
                        cmd =
                            Http.send (fromServer NewStation) Api.getApiTab1
                    in
                        ( s, cmd )

                ShowStation2 ->
                    let
                        cmd =
                            Http.send (fromServer NewStation) Api.getApiTab2
                    in
                        ( s, cmd )

                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            Http.send (fromServer (\id -> NewItem (Item id new))) (postApiItem new)

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
        , div [] []
        ,  button [ onClick (FromUi ShowStation1)] [ text "Station 1"]
        , button [ onClick (FromUi ShowStation2)] [ text "Station 2"]
        , button [ onClick (FromUi ShowStation1)] [ text "Station 3"]
        ]
            ++ [ table []
                    [ thead []
                          [ tr []
                              [ th [ ] [ text "Symbol" ]
                              , th [ ] [ text "Time" ]
                              , th [ ] [ text "Pollution" ]
                              ]
                          ]
                      , tbody []
                          [ tr []
                              [ td [] [ text "NO2" ]
                              , td [ ] [ text (toString (fun state.result)) ]
                              , td [ ] [ text (toString (funNo2 state.result)) ]
                              ]
                              ,
                              tr []
                               [ td [] [ text "SO2" ]
                               , td [ ] [ text (toString (fun state.result)) ]
                               , td [ ] [ text (toString (funSo4 state.result)) ]
                               ]
                          ]
                      ]
               ]

fun : Maybe Station -> Int
fun a =
  case a of
    Nothing -> 1
    Just x -> x.id

funNo2 : Maybe Station -> String
funNo2 a =
  case a of
    Nothing -> "null"
    Just x -> x.no2IndexLevel.indexLevelName

funSo4 : Maybe Station -> String
funSo4 a =
  case a of
    Nothing -> "null"
    Just x -> x.so2IndexLevel.indexLevelName

viewItem : Item -> Html Msg
viewItem item =
    div [] <|
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]
