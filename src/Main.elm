module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, button, div, h2, li, text, ul)
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
    ( Model [], Cmd.none )


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
            [ h2 [] [ text "Get accounts" ]
            , button [ onClick GetAccounts ] [ text "Get accounts" ]
            ]
        , div []
            [ viewAccounts model.accounts ]
        ]
    }


getAccounts : Cmd Msg
getAccounts =
    Http.get
        { url = "http://localhost:5000/accounts"
        , expect = Http.expectJson AccountsReceived (Decode.list accountDecoder)
        }


accountDecoder : Decoder Account
accountDecoder =
    Decode.map6 Account
        (Decode.maybe (Decode.field "aparent_" Decode.string))
        (Decode.field "asubs_" (Decode.list Decode.string))
        (Decode.field "aname" Decode.string)
        (Decode.field "aibalance" (Decode.list balanceDecoder))
        (Decode.field "aebalance" (Decode.list balanceDecoder))
        (Decode.field "anumpostings" Decode.int)


balanceDecoder : Decoder Balance
balanceDecoder =
    Decode.map2 Balance
        (Decode.field "acommodity" Decode.string |> Decode.andThen commodityDecoder)
        (Decode.field "aquantity" quantityDecoder)


commodityDecoder : String -> Decoder Commodity
commodityDecoder string =
    Decode.succeed <|
        case string of
            "€" ->
                Euro

            other ->
                Debug.log other Unknown


quantityDecoder : Decoder Float
quantityDecoder =
    Decode.map2 quantityFromJSON
        (Decode.field "decimalPlaces" Decode.int)
        (Decode.field "decimalMantissa" Decode.int)


quantityFromJSON : Int -> Int -> Float
quantityFromJSON decimalPlaces decimalMantissa =
    toFloat decimalMantissa / toFloat (10 ^ decimalPlaces)


type alias Account =
    { parent : Maybe String
    , children : List String
    , name : String
    , accBalances : List Balance
    , ownBalances : List Balance
    , numPostings : Int
    }


type ParentAccount
    = ParentAccount (Maybe Account)


type ChildrenAccounts
    = ChildrenAccounts (List Account)


type alias Balance =
    { commodity : Commodity
    , quantity : Float
    }


type Commodity
    = Euro
    | Unknown


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
