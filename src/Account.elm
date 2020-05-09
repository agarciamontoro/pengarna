module Account exposing
    ( Account
    , accountDecoder, formatAccountName, viewAccount
    , toDict, totalAssets
    )

{-|

@docs Account


# JSON and HTML

@docs accountDecoder, formatAccountName, viewAccount


# Utils

@docs toDict, totalAssets

-}

import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Dict exposing (Dict)
import Html exposing (Html, a, div, h1, li, nav, text, ul)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode exposing (Decoder)


{-| An Account models an hledger account, containing its full name and its balances.

For example, let's say we have an account `expenses:food:pizza:focaccia` that has a balance of 25€, an account `expenses:food:pizza:margherita` that has a balance of 30€ and the account `expenses:food:pizza`, that has a balance of 45€. The account `expenses:food:pizza` is then represented as follows:

    Account
        Just
        "expenses:food"
        [ "expenses:food:pizza:focaccia", "expenses:food:pizza:margherita" ]
        "expenses:food:pizza"
        [ Balance Commodity.Euro 100 ]
        [ Balance Commodity.Euro 45 ]

-}
type alias Account =
    { parent : Maybe String
    , children : List String
    , name : String
    , accBalances : List Balance
    , ownBalances : List Balance
    }


{-| JSON decoder that expects, at least:

  - An optional `aparent_` field, containing a string with the name of the
    parent account.
  - An `asubs_` field, containing a list of strings with the names of all the
    children accounts.
  - An `aname` field, containing the name of the account; i.e., the last item
    in the colon-separated list of names.
  - An `aibalance`, a list of well-formed balances (see
    [`balanceDecoder`](Balance#balanceDecoder)) that represent the accumulated
    balances; i.e., those of the account itself and those of all of its
    children.
  - An `aebalance`, a list of well-formed balances (see
    [`balanceDecoder`](Balance#balanceDecoder)) that represent the own
    balances; i.e., only those of the account itself.

-}
accountDecoder : Decoder Account
accountDecoder =
    Decode.map5 Account
        (Decode.maybe (Decode.field "aparent_" Decode.string))
        (Decode.field "asubs_" (Decode.list Decode.string))
        (Decode.field "aname" Decode.string)
        (Decode.field "aibalance" (Decode.list Balance.balanceDecoder))
        (Decode.field "aebalance" (Decode.list Balance.balanceDecoder))


getChildren : Account -> Dict String Account -> List Account
getChildren account dict =
    List.filterMap (\name -> Dict.get name dict) account.children


getAllDescendants : Account -> Dict String Account -> List Account
getAllDescendants account dict =
    let
        rec : List Account -> List Account -> List Account
        rec accounts descendants =
            case accounts of
                [] ->
                    descendants

                child :: rest ->
                    let
                        grandchildren =
                            getChildren child dict
                    in
                    rec (List.append rest grandchildren) (List.append descendants grandchildren)
    in
    rec [ account ] []


{-| Given a dictionary of accounts, retrieve the first balance in euros of the
root account "assets".
-}
totalAssets : Dict String Account -> Float
totalAssets accounts =
    Balance.getFirstEuroBalance <|
        case Dict.get "assets" accounts of
            Nothing ->
                []

            Just acc ->
                acc.accBalances


summaryAssets : Dict String Account -> Dict String Float
summaryAssets allAccounts =
    Dict.map
        (\_ value -> Balance.getFirstEuroBalance value.accBalances)
    <|
        Dict.filter (\key _ -> String.startsWith "assets:" key) allAccounts


{-| Convert a list of `Account`s into a `Dict` of `Account`s hashed by their
names.
-}
toDict : List Account -> Dict String Account
toDict list =
    Dict.fromList <| List.map (\acc -> ( acc.name, acc )) list


{-| Render an Account, in the context of a dictionary of accounts (to retrieve
information about its parent and its children), as a list of all the children
accounts with their balances.
-}
viewAccount : Dict String Account -> Account -> List (Html msg)
viewAccount listAccounts account =
    let
        total =
            Balance.getFirstEuroBalance account.accBalances

        sign =
            if total < 0 then
                "-"

            else
                ""

        totalText =
            text <| String.concat [ sign, String.fromFloat total, "€" ]
    in
    [ div [ class Bulma.container ]
        [ div [] [ h1 [ class Bulma.isSize1 ] [ totalText ] ]
        , ul [] <|
            List.map
                (\elem ->
                    li []
                        [ formatAccountName <| Tuple.first elem
                        , text <|
                            String.concat
                                [ String.fromFloat (Tuple.second elem)
                                , "€"
                                ]
                        ]
                )
            <|
                Dict.toList
                    (Dict.filter
                        (\_ balance -> balance /= 0)
                        (summaryAssets <|
                            toDict (getAllDescendants account listAccounts)
                        )
                    )
        ]


capitalizeString : String -> String
capitalizeString string =
    case String.toList string of
        firstLetter :: rest ->
            String.fromList <| Char.toUpper firstLetter :: rest

        [] ->
            string


{-| Render an account name (a String of names separated by colons) as a `nav`
item, linking each part to its own account page.
-}
formatAccountName : String -> Html msg
formatAccountName name =
    nav
        [ BulmaHelpers.classList [ Bulma.breadcrumb, Bulma.hasBulletSeparator ]
        ]
        [ ul [] <|
            List.map
                (\acc ->
                    li []
                        [ a [ href (String.concat [ "/cuentas", acc ]) ]
                            [ text <| capitalizeString <| Maybe.withDefault "" <| List.head (List.reverse (String.split "/" acc)) ]
                        ]
                )
                (accumulatedNames <| name)
        ]


accumulatedNames : String -> List String
accumulatedNames name =
    let
        rec prefix newList list =
            case list of
                x :: xs ->
                    rec
                        (String.concat [ prefix, "/", x ])
                        (List.append
                            newList
                            [ String.concat [ prefix, "/", x ] ]
                        )
                        xs

                [] ->
                    newList
    in
    rec "" [] <| String.split ":" name
