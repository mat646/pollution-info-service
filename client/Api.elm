module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type NoContent = NoContent

type alias Item =
    { id : Int
    , text : String
    }

type alias Index = {
  id : Int,
  indexLevelName : String
  }

type alias Station = {
  id : Int,

  stCalcDate : String,
  stIndexLevel : Index,
  stSourceDataDate : String,

  so2CalcDate : String,
  so2IndexLevel : Index,
  so2SourceDataDate : String,

  no2CalcDate : Int,
  no2IndexLevel : Index,
  no2SourceDataDate : String,

  coCalcDate : String,
  coIndexLevel : Index,
  coSourceDataDate : String,

  pm10CalcDate : String,
  pm10IndexLevel : Index,
  pm10SourceDataDate : String,

  pm25CalcDate : String,
  pm25IndexLevel : Index,
  pm25SourceDataDate : String,

  o3CalcDate : String,
  o3IndexLevel : Index,
  o3SourceDataDate : String,

  c6h6CalcDate : String,
  c6h6IndexLevel : Index,
  c6h6SourceDataDate : String,

  stIndexStatus : Bool,
  stIndexCrParam : String
  }

type alias Table = {
  id : Int,
  time : String,
  so2IndexLevel : String,
  so2Avg : Float,
  no2IndexLevel : String,
  no2Avg : Float,
  coIndexLevel : String,
  coAvg : Float,
  pm10IndexLevel : String,
  pm10Avg : Float,
  pm25IndexLevel : String,
  pm25Avg : Float,
  o3IndexLevel : String,
  o3Avg : Float,
  c6h6IndexLevel : String,
  c6h6Avg : Float
  }

decodeTable : Decoder Table
decodeTable =
    decode Table
        |> required "id" int
        |> required "time" string
        |> required "so2IndexLevel" string
        |> required "so2Avg" float
        |> required "no2IndexLevel" string
        |> required "no2Avg" float
        |> required "coIndexLevel" string
        |> required "coAvg" float
        |> required "pm10IndexLevel" string
        |> required "pm10Avg" float
        |> required "pm25IndexLevel" string
        |> required "pm25Avg" float
        |> required "o3IndexLevel" string
        |> required "o3Avg" float
        |> required "c6h6IndexLevel" string
        |> required "c6h6Avg" float

decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "id" int
        |> required "text" string

decodeIndex : Decoder Index
decodeIndex =
    decode Index
        |> required "id" int
        |> required "indexLevelName" string

decodeTabl : Decoder Station
decodeTabl =
    decode Station
        |> optional "id" int 1
        |> optional "stCalcDate" string "null"
        |> optional "stIndexLevel" decodeIndex (Index 1 "null")
        |> optional "stSourceDataDate" string "null"
        |> optional "so2CalcDate" string "null"
        |> optional "so2IndexLevel" decodeIndex (Index 1 "null")
        |> optional "so2SourceDataDate" string "null"
        |> optional "no2CalcDate" int 1
        |> optional "no2IndexLevel" decodeIndex (Index 1 "null")
        |> optional "no2SourceDataDate" string "null"
        |> optional "coCalcDate" string "null"
        |> optional "coIndexLevel" decodeIndex (Index 1 "null")
        |> optional "coSourceDataDate" string "null"
        |> optional "pm10CalcDate" string "null"
        |> optional "pm10IndexLevel" decodeIndex (Index 1 "null")
        |> optional "pm10SourceDataDate" string "null"
        |> optional "pm25CalcDate" string "null"
        |> optional "pm25IndexLevel" decodeIndex (Index 1 "null")
        |> optional "pm25SourceDataDate" string "null"
        |> optional "o3CalcDate" string "null"
        |> optional "o3IndexLevel" decodeIndex (Index 1 "null")
        |> optional "o3SourceDataDate" string "null"
        |> optional "c6h6CalcDate" string "null"
        |> optional "c6h6IndexLevel" decodeIndex (Index 1 "null")
        |> optional "c6h6SourceDataDate" string "null"
        |> optional "stIndexStatus" bool False
        |> optional "stIndexCrParam" string "null"

getApiTable : Http.Request (Table)
getApiTable =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "test"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeTable
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiTab1 : Http.Request (Station)
getApiTab1 =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "tab1"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeTabl
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiTab2 : Http.Request (Station)
getApiTab2 =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "tab2"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeTabl
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiItem : Http.Request (List (Int))
getApiItem =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiItemByItemId : Int -> Http.Request (Item)
getApiItemByItemId capture_itemId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeItem
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiItem : String -> Http.Request (Int)
postApiItem body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteApiItemByItemId : Int -> Http.Request (NoContent)
deleteApiItemByItemId capture_itemId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "item"
                , capture_itemId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }