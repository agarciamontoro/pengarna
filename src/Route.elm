module Route exposing (Page(..), Route, routeParser)

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
    | NotFound


routeParser : Parser (Page -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Transactions (s "transactions")
        ]


fromUrl : Url -> Maybe Page
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse routeParser


fromRoute : Route -> Maybe Page
fromRoute route =
    fromUrl route.url
