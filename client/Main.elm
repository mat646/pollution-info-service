module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html exposing (program)
import Html.Events exposing (..)
import Html.Attributes exposing (class, value, placeholder, classList)
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
    { error : Maybe String
    , table : Maybe Table
    }


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        fetch =
            Cmd.none

        state =
            { error = Nothing, table = Nothing }
    in
        ( state, fetch )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = NewTable Table


type FromUi
    = GetTable1
    | GetTable2
    | GetTable3


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServerMsg ->
            case fromServerMsg of
                NewTable table ->
                    { s | table = Just (table) } ! []

        FromUi fromUi ->
            case fromUi of
                GetTable1 ->
                    let
                        cmd =
                            Http.send (fromServer NewTable) Api.getApiTable
                    in
                        ( s, cmd )

                GetTable2 ->
                    let
                        cmd =
                            Http.send (fromServer NewTable) Api.getApiTable2
                    in
                        ( s, cmd )

                GetTable3 ->
                    let
                        cmd =
                            Http.send (fromServer NewTable) Api.getApiTable3
                    in
                        ( s, cmd )

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
        , button [ onClick (FromUi GetTable1)] [ text "Station 1"]
        , button [ onClick (FromUi GetTable2)] [ text "Station 2"]
        , button [ onClick (FromUi GetTable3)] [ text "Station 3"]
        ]
            ++ [ table [ class "datagrid" ]
                    [ thead []
                          [ tr []
                              [ th [ ] [ text "Type" ]
                              , th [ ] [ text "Time" ]
                              , th [ ] [ text "Pollution Index" ]
                              , th [ ] [ text "Avg (24h)" ]
                              ]
                          ]
                      , tbody []
                          [ tr []
                              [ td [] [ text "PM10" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funPm10 state.table)) ]
                              , td [ ] [ text (toString (funAvgPm10 state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "PM25" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funPm25 state.table)) ]
                              , td [ ] [ text (toString (funAvgPm25 state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "NO2" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funNo2 state.table)) ]
                              , td [ ] [ text (toString (funAvgNo2 state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "SO2" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funSo2 state.table)) ]
                              , td [ ] [ text (toString (funAvgSo2 state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "CO" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funCo state.table)) ]
                              , td [ ] [ text (toString (funAvgCo state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "O3" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funO3 state.table)) ]
                              , td [ ] [ text (toString (funAvgO3 state.table)) ]
                              ]
                              ,
                              tr []
                              [ td [] [ text "C6H6" ]
                              , td [ ] [ text (toString (fun state.table)) ]
                              , td [ ] [ text (toString (funC6H6 state.table)) ]
                              , td [ ] [ text (toString (funAvgC6H6 state.table)) ]
                              ]
                          ]
                      ]
               ]



fun : Maybe Table -> String
fun a =
  case a of
    Nothing -> "null"
    Just x -> x.time

funNo2 : Maybe Table -> String
funNo2 a =
  case a of
    Nothing -> "null"
    Just x -> x.no2IndexLevel

funAvgNo2 : Maybe Table -> Float
funAvgNo2 a =
  case a of
    Nothing -> 0.0
    Just x -> x.no2Avg

funSo2 : Maybe Table -> String
funSo2 a =
  case a of
    Nothing -> "null"
    Just x -> x.so2IndexLevel

funAvgSo2 : Maybe Table -> Float
funAvgSo2 a =
  case a of
    Nothing -> 0.0
    Just x -> x.so2Avg

funCo : Maybe Table -> String
funCo a =
  case a of
    Nothing -> "null"
    Just x -> x.coIndexLevel

funAvgCo : Maybe Table -> Float
funAvgCo a =
  case a of
    Nothing -> 0.0
    Just x -> x.coAvg

funPm10 : Maybe Table -> String
funPm10 a =
  case a of
    Nothing -> "null"
    Just x -> x.pm10IndexLevel

funAvgPm10 : Maybe Table -> Float
funAvgPm10 a =
  case a of
    Nothing -> 0.0
    Just x -> x.pm10Avg

funPm25 : Maybe Table -> String
funPm25 a =
  case a of
    Nothing -> "null"
    Just x -> x.pm25IndexLevel

funAvgPm25 : Maybe Table -> Float
funAvgPm25 a =
  case a of
    Nothing -> 0.0
    Just x -> x.pm25Avg

funO3 : Maybe Table -> String
funO3 a =
  case a of
    Nothing -> "null"
    Just x -> x.o3IndexLevel

funAvgO3 : Maybe Table -> Float
funAvgO3 a =
  case a of
    Nothing -> 0.0
    Just x -> x.o3Avg

funC6H6 : Maybe Table -> String
funC6H6 a =
  case a of
    Nothing -> "null"
    Just x -> x.c6h6IndexLevel

funAvgC6H6 : Maybe Table -> Float
funAvgC6H6 a =
  case a of
    Nothing -> 0.0
    Just x -> x.c6h6Avg