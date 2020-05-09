module Main exposing (main)

{-|

@docs main

-}

import Account exposing (Account)
import Api
import Browser
import Browser.Navigation as Nav
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Dict exposing (Dict)
import Html exposing (Html, a, div, h1, nav, section, span, text)
import Html.Attributes exposing (class, classList, height, href, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Route exposing (Page)
import Svg exposing (circle, svg)
import Svg.Attributes as SvgAttrs
import Task
import Time
import TimeUtils
import Transaction exposing (Transaction)
import Url


{-| Entry point of the application, modelled as a Browser.application
-}
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
init _ url key =
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
subscriptions _ =
    Sub.none



-- Commands


getAccounts : Cmd Msg
getAccounts =
    Http.get
        { url = Api.url "accounts"
        , expect =
            Http.expectJson AccountsReceived <|
                Decode.list Account.accountDecoder
        }


getTransactions : Cmd Msg
getTransactions =
    Http.get
        { url = Api.url "transactions"
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
            viewNotFound


viewHome : Model -> Html Msg
viewHome model =
    div [ class Bulma.columns ]
        [ div [ BulmaHelpers.classList [ Bulma.column, Bulma.isOneThird ] ]
            [ viewAllAccounts model.accounts ]
        , div [ class Bulma.column ]
            [ viewMonthlySummary model ]
        ]


viewAllAccounts : Dict String Account -> Html Msg
viewAllAccounts accounts =
    Maybe.map
        (Account.viewAccount accounts)
        (Dict.get
            "assets"
            accounts
        )
        |> Maybe.withDefault (div [] [])


viewMonthlySummary : Model -> Html Msg
viewMonthlySummary model =
    div []
        [ text (Maybe.map TimeUtils.monthToSpanish model.currentMonth |> Maybe.withDefault "No month")
        ]


viewTransactions : Model -> Html Msg
viewTransactions model =
    Transaction.viewTransactionList model.transactions


viewBalances : Model -> Html Msg
viewBalances model =
    Transaction.balancesFromMaybeMonth model.currentMonth model.transactions


viewAccountPage : String -> Model -> Html Msg
viewAccountPage accName model =
    Maybe.withDefault (div [] []) <|
        Maybe.map
            (Account.viewAccount model.accounts)
            (Dict.get accName model.accounts)


viewNotFound : Html Msg
viewNotFound =
    h1 [ class Bulma.isSize1 ] [ text "Not found." ]


view : Model -> Browser.Document Msg
view model =
    { title = "Pengarna"
    , body =
        [ viewNavbar model
        , section [ class Bulma.section ]
            [ div [ class Bulma.container ]
                [ viewPage model ]
            ]
        ]
    }
