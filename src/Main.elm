module Main exposing (main)

import Account exposing (Account)
import Browser
import Browser.Navigation as Nav
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Commodity exposing (Commodity)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, nav, p, section, span, strong, text, ul)
import Html.Attributes exposing (class, classList, height, href, src, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Route exposing (Page, Route)
import Svg exposing (circle, svg)
import Svg.Attributes as SvgAttrs
import Task
import Time
import Transaction exposing (Transaction)
import Url



-- Main entry


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Data types


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AccountsReceived (Result Http.Error (List Account))
    | TransactionsReceived (Result Http.Error (List Transaction))
    | NewTime Time.Posix
    | ToggleMenu


type alias Model =
    { route : Page
    , navKey : Nav.Key
    , accounts : Dict String Account
    , transactions : List Transaction
    , currentMonth : Maybe Time.Month
    , isMenuActive : Bool
    }



-- Initialization


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initPage =
            case Route.fromUrl url of
                Just page ->
                    page

                Nothing ->
                    Route.Home
    in
    ( Model initPage key Dict.empty [] Nothing False
    , Cmd.batch
        [ getAccounts
        , getTransactions
        , Task.perform NewTime Time.now
        ]
    )



-- Subscriptions management


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Commands


getAccounts : Cmd Msg
getAccounts =
    Http.get
        { url = "http://localhost:5000/accounts"
        , expect =
            Http.expectJson AccountsReceived <|
                Decode.list Account.accountDecoder
        }


getTransactions : Cmd Msg
getTransactions =
    Http.get
        { url = "http://localhost:5000/transactions"
        , expect =
            Http.expectJson TransactionsReceived <|
                Decode.list Transaction.transactionDecoder
        }



-- Update management


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Route.fromUrl url of
                Just page ->
                    ( { model | route = page, isMenuActive = False }, Cmd.none )

                Nothing ->
                    ( { model | route = Route.NotFound, isMenuActive = False }, Cmd.none )

        AccountsReceived result ->
            case result of
                Ok accountsList ->
                    ( { model | accounts = Account.toDict accountsList }, Cmd.none )

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

        ToggleMenu ->
            ( { model | isMenuActive = not model.isMenuActive }
            , Cmd.none
            )



-- Views


brandCircle : Html Msg
brandCircle =
    svg
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


isActive : Page -> Model -> Bool
isActive page model =
    model.route == page


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav [ class Bulma.navbar ]
        [ div [ class Bulma.navbarBrand ]
            [ a [ class Bulma.navbarItem, href "/" ] [ brandCircle ]
            , div [ classList [ ( Bulma.navbarBurger, True ), ( Bulma.isActive, model.isMenuActive ) ], onClick ToggleMenu ]
                [ span [] []
                , span [] []
                , span [] []
                ]
            ]
        , div [ classList [ ( Bulma.navbarMenu, True ), ( Bulma.isActive, model.isMenuActive ) ] ]
            [ div [ class Bulma.navbarStart ]
                [ a
                    [ classList
                        [ ( Bulma.navbarItem, True )
                        , ( Bulma.isActive, isActive Route.Home model )
                        ]
                    , href "/"
                    ]
                    [ text "Inicio" ]
                , a
                    [ classList
                        [ ( Bulma.navbarItem, True )
                        , ( Bulma.isActive, isActive Route.Transactions model )
                        ]
                    , href "/transacciones"
                    ]
                    [ text "Transacciones" ]
                , a
                    [ classList
                        [ ( Bulma.navbarItem, True )
                        , ( Bulma.isActive, isActive Route.Balance model )
                        ]
                    , href "/balance"
                    ]
                    [ text "Balance" ]
                ]
            ]
        ]


viewTotal : Model -> Html Msg
viewTotal model =
    let
        total =
            Account.totalAssets model.accounts

        sign =
            if total < 0 then
                "-"

            else
                ""
    in
    text <|
        String.concat
            [ sign
            , String.fromFloat total
            , "€"
            ]


viewPage : Model -> Html Msg
viewPage model =
    case model.route of
        Route.Home ->
            viewHome model

        Route.Transactions ->
            viewTransactions model

        Route.Balance ->
            viewBalances model

        Route.Account accName ->
            viewAccountPage accName model

        Route.NotFound ->
            viewNotFound model


viewHome : Model -> Html Msg
viewHome model =
    section [ class Bulma.section ]
        [ div [ class Bulma.container ]
            [ div [] [ h1 [ class Bulma.isSize1 ] [ viewTotal model ] ]
            , ul [] <|
                List.map
                    (\elem ->
                        li []
                            [ Account.formatAccountName <| String.dropLeft 7 <| Tuple.first elem
                            , text <|
                                String.concat
                                    [ String.fromFloat (Tuple.second elem)
                                    , "€"
                                    ]
                            ]
                    )
                <|
                    Dict.toList (Account.summaryAssets model.accounts)
            ]
        ]


viewTransactions : Model -> Html Msg
viewTransactions model =
    section [ class Bulma.section ]
        [ div [ class Bulma.container ]
            [ div [ BulmaHelpers.classList [ Bulma.column ] ] <|
                Transaction.viewTransactionList model.transactions
            ]
        ]


viewBalances : Model -> Html Msg
viewBalances model =
    section [ class Bulma.section ]
        [ div [ class Bulma.container ]
            [ div [ BulmaHelpers.classList [ Bulma.column ] ] <|
                Transaction.balancesFromMonth model.currentMonth model.transactions
            ]
        ]


viewAccountPage : String -> Model -> Html Msg
viewAccountPage accName model =
    section [] [ text accName ]


viewNotFound : Model -> Html Msg
viewNotFound model =
    section [ class Bulma.section ]
        [ div [ class Bulma.container ]
            [ h1 [ class Bulma.isSize1 ] [ text "Not found." ] ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Pengar"
    , body =
        [ viewNavbar model
        , viewPage model
        ]
    }
