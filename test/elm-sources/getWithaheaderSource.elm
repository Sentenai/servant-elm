module GetWithAHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : Maybe (String) -> Maybe (Int) -> String -> Int -> Http.Request (Http.Response String)
getWithaheader header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (Http.header "MyIntHeader" << String.fromInt) header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (Http.header "MyRequiredIntHeader" << String.fromInt) (Just header_MyRequiredIntHeader)
                ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString string res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
