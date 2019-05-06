module GitHub exposing (Repo, User, getRepo, getUser)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias User =
    { login : String
    , name : String
    , avatar : String
    , url : String
    , followers : Int
    , following : Int
    , repoCnt : Int
    , bio : String
    }


type alias Repo =
    { name : String
    , owner : String
    , avatar : String
    , url : String
    , description : String
    , starCnt : Int
    , forkCnt : Int
    , language : String
    }


decodeUser : Decoder User
decodeUser =
    Decode.map8 User
        (Decode.field "login" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "html_url" Decode.string)
        (Decode.field "followers" Decode.int)
        (Decode.field "following" Decode.int)
        (Decode.field "public_repos" Decode.int)
        (Decode.field "bio" Decode.string)


decodeRepo : Decoder Repo
decodeRepo =
    Decode.map8 Repo
        (Decode.field "name" Decode.string)
        (Decode.at [ "owner", "login" ] Decode.string)
        (Decode.at [ "owner", "avatar_url" ] Decode.string)
        (Decode.field "html_url" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "stargazers_count" Decode.int)
        (Decode.field "forks_count" Decode.int)
        (Decode.field "language" Decode.string)


getUser : String -> (Result Http.Error User -> msg) -> Cmd msg
getUser owner toMsg =
    Http.get
        { url = String.concat [ baseUrl, "users/", owner ]
        , expect = Http.expectJson toMsg decodeUser
        }


getRepo : String -> String -> (Result Http.Error Repo -> msg) -> Cmd msg
getRepo owner name toMsg =
    Http.get
        { url = String.concat [ baseUrl, "repos/", owner, "/", name ]
        , expect = Http.expectJson toMsg decodeRepo
        }


baseUrl : String
baseUrl =
    "https://api.github.com/"
