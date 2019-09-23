module Account exposing
    ( Account
    , accountDecoder
    , getAccountLineage
    , totalAssets
    , totalAssetsWithDefault
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


totalAssets : List Account -> Maybe Float
totalAssets accounts =
    Maybe.andThen
        Balance.getEuroBalance
        (Maybe.map
            .accBalances
            (List.head (List.filter (\account -> account.name == "assets") accounts))
        )


totalAssetsWithDefault : Float -> List Account -> Float
totalAssetsWithDefault default accounts =
    Maybe.withDefault default (totalAssets accounts)
