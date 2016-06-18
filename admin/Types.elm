module Types exposing (..)


import Maybe as Maybe
import ISO8601 as ISO
import Dict as Dict
import Http as Http

import Json.Decode as Json exposing (..)

type alias Model = { posts : Maybe(Dict.Dict String Post) }

type alias Post =
  { slug : String
  , uuid : String
  , title : String
  , html : String
  , markdown : String
  , publishedAt : Maybe(ISO.Time)
  }

type Msg
  = FetchPosts
  | FetchPost String
  | GotPosts (List Post)
  | GotPost Post
  | FetchFail Http.Error
