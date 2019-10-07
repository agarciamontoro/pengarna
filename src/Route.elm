module Route exposing (Page(..), Route, fromUrl, routeParser)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type alias Route =
    { url : Url.Url
    , key : Nav.Key
    }


type Page
    = Home
    | Transactions
    | Balance
    | NotFound


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Transactions (s "transacciones")
        , Parser.map Balance (s "balance")
        ]


fromUrl : Url -> Maybe Page
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    Parser.parse routeParser url


fromRoute : Route -> Maybe Page
fromRoute route =
    fromUrl route.url
