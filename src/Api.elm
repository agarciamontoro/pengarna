module Api exposing (url)


host : String
host =
    "http://192.168.1.59:5000/"


url : String -> String
url str =
    host ++ str
