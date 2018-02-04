module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type NoContent = NoContent

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
                , "tab1"
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

getApiTable2 : Http.Request (Table)
getApiTable2 =
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
            Http.expectJson decodeTable
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiTable3 : Http.Request (Table)
getApiTable3 =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "tab3"
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