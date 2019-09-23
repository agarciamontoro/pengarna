module Main exposing (main)

import Account exposing (Account)
import Browser
import Browser.Navigation
import Commodity exposing (Commodity)
import Html exposing (Html, button, div, h1, h2, li, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Url


type Msg
    = GetAccounts
    | AccountsReceived (Result Http.Error (List Account))


type alias Model =
    { accounts : List Account }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( Model [], getAccounts )


onUrlChange : Url.Url -> Msg
onUrlChange url =
    GetAccounts


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest request =
    Debug.todo "onUrlRequest not implemented"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAccounts ->
            ( model, getAccounts )

        AccountsReceived result ->
            case result of
                Ok accountsList ->
                    ( { model | accounts = accountsList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


viewAccounts : List Account -> Html Msg
viewAccounts accounts =
    let
        getNameLi : Account -> Html Msg
        getNameLi acc =
            li [] [ text acc.name ]
    in
    ul [] (List.map getNameLi accounts)


view : Model -> Browser.Document Msg
view model =
    { title = "Pengar"
    , body =
        [ div []
            [ h1 [] [ text <| String.fromFloat (Account.totalAssetsWithDefault 0 model.accounts) ] ]
        , div []
            [ viewAccounts model.accounts ]
        ]
    }


getAccounts : Cmd Msg
getAccounts =
    Http.get
        { url = "http://localhost:5000/accounts"
        , expect = Http.expectJson AccountsReceived (Decode.list Account.accountDecoder)
        }


{-| Main entry
-}
main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
