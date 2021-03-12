module Util.Assets exposing (assetPath, clockTimer, serverDown)


type Asset
    = Image String


withBasePath : String -> Asset
withBasePath path =
    Image ("./assets/" ++ path)


assetPath : Asset -> String
assetPath asset =
    case asset of
        Image string ->
            string


serverDown : Asset
serverDown =
    withBasePath "undraw_server_down_s4lk.svg"


clockTimer : Asset
clockTimer =
    withBasePath "undraw_season_change_f99v.svg"
