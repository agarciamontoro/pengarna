module Main exposing (main)

import Account exposing (Account)
import Browser
import Browser.Navigation
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Commodity exposing (Commodity)
import Html exposing (Html, button, div, h1, h2, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Task
import Time
import Transaction exposing (Transaction)
import Url


type Msg
    = GetAccounts
    | AccountsReceived (Result Http.Error (List Account))
    | TransactionsReceived (Result Http.Error (List Transaction))
    | NewTime Time.Posix


type alias Model =
    { accounts : List Account
    , transactions : List Transaction
    , currentMonth : Maybe Time.Month
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( Model [] [] Nothing
    , Cmd.batch
        [ getAccounts
        , getTransactions
        , Task.perform NewTime Time.now
        ]
    )


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

        TransactionsReceived result ->
            case result of
                Ok transactionsList ->
                    ( { model | transactions = transactionsList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NewTime now ->
            ( { model | currentMonth = Just <| Time.toMonth Time.utc now }
            , Cmd.none
            )


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
    let
        total =
            Account.totalAssetsWithDefault 0 model.accounts

        sign =
            if total < 0 then
                "-"

            else
                "+"
    in
    { title = "Pengar"
    , body =
        [ section [ BulmaHelpers.classList [ Bulma.section, Bulma.columns ] ]
            [ div [ BulmaHelpers.classList [ Bulma.column, Bulma.isNarrow ] ]
                [ div [ class Bulma.container ]
                    [ h1 [ class Bulma.isSize1 ]
                        [ text <|
                            String.concat
                                [ sign
                                , String.fromFloat total
                                , "€"
                                ]
                        ]
                    ]
                ]
            , div [ BulmaHelpers.classList [ Bulma.column ] ]
                [ ul [] <|
                    currentMonthTransactions model
                ]
            ]
        ]
    }


currentMonthTransactions : Model -> List (Html msg)
currentMonthTransactions model =
    case model.currentMonth of
        Nothing ->
            []

        Just month ->
            [ ul [] <|
                List.map
                    (\trans -> li [] (Transaction.viewTransaction trans))
                    (Transaction.filterByMonth model.transactions month)
            ]


getAccounts : Cmd Msg
getAccounts =
    Http.get
        { url = "http://localhost:5000/accounts"
        , expect = Http.expectJson AccountsReceived (Decode.list Account.accountDecoder)
        }


getTransactions : Cmd Msg
getTransactions =
    Http.get
        { url = "http://localhost:5000/transactions"
        , expect = Http.expectJson TransactionsReceived (Decode.list Transaction.transactionDecoder)
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
