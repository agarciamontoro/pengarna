module Main exposing (main)

import Account exposing (Account)
import Browser
import Browser.Navigation
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Commodity exposing (Commodity)
import Dict
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, nav, p, section, span, strong, text, ul)
import Html.Attributes exposing (class, height, src, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (circle, svg)
import Svg.Attributes as SvgAttrs
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


viewNavbar : Html Msg
viewNavbar =
    nav [ class Bulma.navbar ]
        [ div [ class Bulma.navbarBrand ]
            [ a [ class Bulma.navbarItem ]
                [ svg
                    [ SvgAttrs.width "2em"
                    , SvgAttrs.height "2em"
                    , SvgAttrs.viewBox "0 0 100 100"
                    ]
                    [ circle
                        [ SvgAttrs.cx "50"
                        , SvgAttrs.cy "50"
                        , SvgAttrs.r "50"
                        ]
                        []
                    ]
                ]
            , a [ BulmaHelpers.classList [ Bulma.navbarBurger ] ]
                [ span [] []
                , span [] []
                , span [] []
                ]
            ]
        , div [ class Bulma.navbarMenu ]
            [ div [ class Bulma.navbarStart ]
                [ a [ class Bulma.navbarItem ] [ text "Inicio" ]
                , a [ class Bulma.navbarItem ] [ text "Transacciones" ]

                -- , div [ BulmaHelpers.classList [ Bulma.navbarItem, Bulma.hasDropdown, Bulma.isHoverable ] ]
                --     [ a [ class Bulma.navbarLink ] [ text "More" ]
                --     , div [ class Bulma.navbarDropdown ]
                --         [ a [ class Bulma.navbarItem ] [ text "About" ]
                --         , a [ class Bulma.navbarItem ] [ text "Jobs" ]
                --         , a [ class Bulma.navbarItem ] [ text "Contact" ]
                --         , hr [ class Bulma.navbarDivider ] []
                --         , a [ class Bulma.navbarItem ] [ text "Report an issue" ]
                --         ]
                --     ]
                ]

            -- , div [ class Bulma.navbarEnd ]
            --     [ div [ class Bulma.navbarItem ]
            --         [ div [ class Bulma.buttons ]
            --             [ a [ BulmaHelpers.classList [ Bulma.button, Bulma.isPrimary ] ] [ strong [] [ text "Sign up" ] ]
            --             , a [ BulmaHelpers.classList [ Bulma.button, Bulma.isLight ] ] [ text "Log in" ]
            --             ]
            --         ]
            --     ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        total =
            Account.totalAssets model.accounts

        sign =
            if total < 0 then
                "-"

            else
                ""
    in
    { title = "Pengar"
    , body =
        [ viewNavbar
        , section [ BulmaHelpers.classList [ Bulma.section, Bulma.columns ] ]
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
            , div [ BulmaHelpers.classList [ Bulma.column ] ] <|
                List.append
                    (Transaction.viewTransactionList model.transactions)
                    (Transaction.balancesFromMonth model.currentMonth model.transactions)
            ]
        ]
    }


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
