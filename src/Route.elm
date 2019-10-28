module Route exposing (Page(..), Route, fromUrl, routeParser)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type alias Route =
    { url : Url.Url
    , key : Nav.Key
    }


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


fromRoute : Route -> Maybe Page
fromRoute route =
    fromUrl route.url
