module Api exposing (url)

{-| Define the host URL and functions to build API URLs.

@docs url

-}


host : String
host =
    "http://192.168.1.59:5000/"


{-| Given a path in a String, prepend the API host to it.
-}
url : String -> String
url str =
    host ++ str
