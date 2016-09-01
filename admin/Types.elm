module Types exposing (..)


import Maybe as Maybe
import ISO8601 as ISO
import Dict as Dict
import Http as Http

import Json.Decode as Json exposing (..)

type alias Model =
  { posts : Maybe(Dict.Dict String Post)
  , selectedPost : Maybe Post
  , activeView : ActiveView
  , tags : Maybe(List Tag)
  }

type alias Post =
  { slug : String
  , uuid : String
  , title : String
  , html : String
  , markdown : String
  , publishedAt : Maybe(ISO.Time)
  , updatedAt : ISO.Time
  , id : Int
  }

type alias Tag =
  { name : String
  , slug : String
  , uuid : String
  , postIds : List Int
  , id : Int
  , createdAt : ISO.Time
  }

type Msg
  -- = FetchPosts
  = FetchPost String
  | GotPost Post
  | FetchFail Http.Error
  | SelectPost Post
  | EditPost Post
  | InputEditPost String
  | GotTags (List Tag)
  | GotInitialData (List Post, List Tag)

type ActiveView
  = Index
  | Edit
