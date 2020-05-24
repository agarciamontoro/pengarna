module Main exposing (main)

{-|

@docs main

-}

import Account exposing (Account, SimpleAccount)
import Api
import Browser
import Browser.Navigation as Nav
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Dict exposing (Dict)
import Html exposing (Html, a, div, figure, h1, hr, i, li, nav, section, span, text, ul)
import Html.Attributes exposing (class, classList, height, href, width)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import PieChart
import Route exposing (Page)
import Svg exposing (circle, svg)
import Svg.Attributes as SvgAttrs
import Task
import Time
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
    | CashflowReceived (Result Http.Error (List SimpleAccount))
    | ExpenseSummaryReceived (Result Http.Error (List SimpleAccount))
    | NewTime Time.Posix
    | ToggleMenu
    | PieChartMsg PieChart.Msg


type alias Model =
    { route : Page
    , navKey : Nav.Key
    , accounts : Dict String Account
    , transactions : List Transaction
    , currentMonth : Maybe Time.Month
    , isMenuActive : Bool
    , cashflow : List SimpleAccount
    , summary : List SimpleAccount
    , pieChart : PieChart.Model
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
    ( Model initPage key Dict.empty [] Nothing False [] [] (PieChart.getModel [])
    , Cmd.batch
        [ getAccounts
        , getTransactions
        , getCashflow
        , getExpenseSummary
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
        { url = Api.url "/accounts"
        , expect =
            Http.expectJson AccountsReceived <|
                Decode.list Account.accountDecoder
        }


getTransactions : Cmd Msg
getTransactions =
    Http.get
        { url = Api.url "/transactions"
        , expect =
            Http.expectJson TransactionsReceived <|
                Decode.list Transaction.transactionDecoder
        }


getCashflow : Cmd Msg
getCashflow =
    Http.get
        { url = Api.url "/cashflow"
        , expect =
            Http.expectJson CashflowReceived <|
                Decode.field "accounts" <|
                    Decode.list Account.simpleDecoder
        }


getExpenseSummary : Cmd Msg
getExpenseSummary =
    Http.get
        { url = Api.url "/expense-summary"
        , expect =
            Http.expectJson ExpenseSummaryReceived <|
                Decode.field "accounts" <|
                    Decode.list Account.simpleDecoder
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

        CashflowReceived result ->
            case result of
                Ok cashflow ->
                    ( { model | cashflow = cashflow }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ExpenseSummaryReceived result ->
            case result of
                Ok summary ->
                    ( { model | summary = summary, pieChart = PieChart.getModel summary }, Cmd.none )

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

        PieChartMsg message ->
            ( { model | pieChart = PieChart.update message model.pieChart }, Cmd.none )



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
            [ viewCashflow model.cashflow ]
        , div [ class Bulma.column ]
            [ viewExpenseSummary model ]
        ]


viewCashflow : List SimpleAccount -> Html Msg
viewCashflow accounts =
    case accounts of
        total :: rest ->
            div [ class Bulma.container ]
                [ div [] [ h1 [ class Bulma.isSize1 ] [ text <| "Activos" ] ]
                , ul [ class Bulma.isClearfix ] <|
                    List.map
                        (\elem ->
                            li [ class Bulma.isClearfix ]
                                [ div [ class Bulma.isPulledLeft ] [ Account.formatAccountName <| String.replace "assets:" "" elem.name ]
                                , div [ class Bulma.isPulledRight ] [ text <| formatBalance elem.balance ]
                                ]
                        )
                        rest
                , div [] [ h1 [ BulmaHelpers.classList [ Bulma.isSize1, Bulma.isPulledRight ] ] [ text <| formatBalance total.balance ] ]
                ]

        [] ->
            div [] []


formatBalance : Int -> String
formatBalance balance =
    let
        str =
            String.fromInt balance

        maybeWholePart =
            String.slice 0 -2 str

        wholePart =
            if String.isEmpty maybeWholePart then
                "0"

            else
                maybeWholePart
    in
    String.concat
        [ wholePart
        , ","
        , String.right 2 str
        , "€"
        ]


icon : String -> Html Msg
icon name =
    i [ class <| "fas fa-" ++ name ] []


viewExpenseSummary : Model -> Html Msg
viewExpenseSummary model =
    let
        name elem =
            String.replace "expenses:" "" elem.name

        f elem =
            li [ class Bulma.isClearfix ]
                [ div [ BulmaHelpers.classList [ Bulma.isPulledLeft, Bulma.isFlex ] ]
                    [ span [ class Bulma.icon ] [ icon <| Account.iconFromName <| name elem ]
                    , Account.formatAccountName <| name elem
                    ]
                , div [ BulmaHelpers.classList [ Bulma.isPulledRight, Bulma.isFamilyMonospace ] ] [ text <| formatBalance elem.balance ]
                ]
    in
    case model.summary of
        total :: rest ->
            div [ class Bulma.container ]
                [ div [] [ h1 [ class Bulma.isSize1 ] [ text <| "Gastos" ] ]
                , ul [] <| List.map f <| List.sortBy (.balance >> (*) -1) rest
                , hr [] []
                , div [] [ h1 [ BulmaHelpers.classList [ Bulma.isSize1, Bulma.isPulledRight ] ] [ text <| formatBalance total.balance ] ]
                , figure [] [ PieChart.view model.pieChart ] |> Html.map toMsg
                ]

        [] ->
            div [] []


toMsg : PieChart.Msg -> Msg
toMsg msg =
    PieChartMsg msg


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
