module Api exposing (url)

{-| Define the host URL and functions to build API URLs.

@docs url

-}


host : String
host =
    "http://localhost:8080"


{-| Given a path in a String, prepend the API host to it.
-}
url : String -> String
url str =
    host ++ str
