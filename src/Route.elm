module Route exposing
    ( Route
    , Page(..), fromUrl
    )

{-| The Route module exposes types and functions to manage routing and pages,
defining the correctly formed URLs and the Pages they point to.


# Route

@docs Route


# Page

@docs Page, fromUrl

-}

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


{-| A Route holds the URL and the navigation key.
-}
type alias Route =
    { url : Url.Url
    , key : Nav.Key
    }


{-| The Page is an enumeration of all the supported pages in the application.
-}
type Page
    = Home
    | Transactions
    | Account String
    | Balance
    | NotFound


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Transactions (s "transacciones")
        , Parser.map Balance (s "balances")
        ]


{-| Try to convert an Url into a Page, using the following mapping:

    /                  -> Just Home
    /transacciones     -> Just Transactions
    /cuentas/acct/name -> Just (Account "acct:name")
    /balances          -> Just Balance
    /any/other/route   -> Nothing

-}
fromUrl : Url -> Maybe Page
fromUrl url =
    -- After /cuentas/, an account is encoded as a path, with each one of the
    -- names separated by a /
    if String.startsWith "/cuentas/" url.path then
        Just <|
            Account <|
                String.replace "/" ":" <|
                    String.dropLeft (String.length "/cuentas/") (Maybe.withDefault "" (Url.percentDecode url.path))

    else
        Parser.parse routeParser url
