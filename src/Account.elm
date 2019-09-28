module Account exposing
    ( Account
    , accountDecoder
    , getAccountLineage
    , totalAssets
    )

import Balance exposing (Balance)
import Commodity exposing (Commodity)
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


totalAssets : List Account -> Float
totalAssets accounts =
    Balance.getFirstEuroBalance
        (List.concatMap
            .accBalances
            (List.filter (\account -> account.name == "assets") accounts)
        )
