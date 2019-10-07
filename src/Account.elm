module Account exposing
    ( Account
    , accountDecoder
    , formatAccountName
    , getAccountLineage
    , summaryAssets
    , toDict
    , totalAssets
    , viewAccounts
    )

import Balance exposing (Balance)
import Bulma.Classes as Bulma
import Bulma.Helpers as BulmaHelpers
import Commodity exposing (Commodity)
import Dict exposing (Dict)
import Html exposing (Html, a, li, nav, text, ul)
import Json.Decode as Decode exposing (Decoder)


type alias Account =
    { parent : Maybe String
    , children : List String
    , name : String
    , accBalances : List Balance
    , ownBalances : List Balance
    , numPostings : Int
    }


accountDecoder : Decoder Account
accountDecoder =
    Decode.map6 Account
        (Decode.maybe (Decode.field "aparent_" Decode.string))
        (Decode.field "asubs_" (Decode.list Decode.string))
        (Decode.field "aname" Decode.string)
        (Decode.field "aibalance" (Decode.list Balance.balanceDecoder))
        (Decode.field "aebalance" (Decode.list Balance.balanceDecoder))
        (Decode.field "anumpostings" Decode.int)


getAccountLineage : Account -> List String
getAccountLineage account =
    String.split ":" account.name


getParent : Account -> Dict String Account -> Maybe Account
getParent account dict =
    Maybe.andThen (\name -> Dict.get name dict) account.parent


getChildren : Account -> Dict String Account -> List Account
getChildren account dict =
    List.filterMap (\name -> Dict.get name dict) account.children


totalAssets : Dict String Account -> Float
totalAssets accounts =
    Balance.getFirstEuroBalance
        (List.concatMap
            .accBalances
            (List.filter (\account -> account.name == "assets") <|
                Dict.values accounts
            )
        )


summaryAssets : Dict String Account -> Dict String Float
summaryAssets allAccounts =
    Dict.map
        (\key value -> Balance.getFirstEuroBalance value.accBalances)
    <|
        Dict.filter (\key value -> String.startsWith "assets:" key) allAccounts


toDict : List Account -> Dict String Account
toDict list =
    Dict.fromList <| List.map (\acc -> ( acc.name, acc )) list


viewAccounts : List Account -> Html msg
viewAccounts accounts =
    let
        getNameLi : Account -> Html msg
        getNameLi acc =
            li [] [ text acc.name ]
    in
    ul [] (List.map getNameLi accounts)


capitalizeString : String -> String
capitalizeString string =
    case String.toList string of
        firstLetter :: rest ->
            String.fromList <| Char.toUpper firstLetter :: rest

        [] ->
            string


formatAccountName : String -> Html msg
formatAccountName name =
    let
        accounts =
            String.split ":" name
    in
    nav [ BulmaHelpers.classList [ Bulma.breadcrumb, Bulma.hasBulletSeparator ] ]
        [ ul [] <|
            List.map (\acc -> li [] [ a [] [ text <| capitalizeString acc ] ]) accounts
        ]
