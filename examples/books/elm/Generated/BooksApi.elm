module Generated.BooksApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String.Conversions as String
import Url


type alias Book =
    { name : String
    }

decodeBook : Decoder Book
decodeBook =
    succeed Book
        |> required "name" string

encodeBook : Book -> Json.Encode.Value
encodeBook x =
    Json.Encode.object
        [ ( "name", Json.Encode.string x.name )
        ]

postBooks : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Book -> Cmd msg
postBooks toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getBooks : (Result (Maybe (Http.Metadata, String), Http.Error) (List (Book)) -> msg) -> Cmd msg
getBooks toMsg =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
                , "books"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString (list decodeBook) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }

getBooksByBookId : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Int -> Cmd msg
getBooksByBookId toMsg capture_bookId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8000"
                , "books"
                , capture_bookId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }