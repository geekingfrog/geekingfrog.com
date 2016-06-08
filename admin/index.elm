module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Decode as Json exposing (..)
import Task as Task
import Debug as Debug
import Dict as Dict exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Model = { posts : Dict.Dict String Post }

type alias Post =
  { slug : String
  , uuid : String
  , title : String
  , html : String
  , markdown : String
  }

type Msg
  = FetchPosts
  | FetchPost String
  | GotPosts (List Post)
  | GotPost Post
  | FetchFail Http.Error

init : (Model, Cmd Msg)
init = ({ posts = Dict.empty }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchPosts ->
      (model, getAllPosts)
    FetchFail err ->
      Debug.log (toString err) (model, Cmd.none)
    FetchPost postSlug ->
      (model, getOnePost postSlug)
    GotPosts posts ->
      ({model |
        posts = Dict.fromList (List.map (\p -> (p.slug, p)) posts)}
      , Cmd.none)
    GotPost post ->
      ({model | posts = Dict.insert post.slug post model.posts}, Cmd.none)

view model =
  div []
    [ button [ onClick FetchPosts ] [ text "fetch all posts" ]
    , button [ onClick (FetchPost "struggles-with-parsing-json-with-aeson")] [ text "fetch one post" ]
    , Html.ul [] (List.map (snd >> renderPost) (Dict.toList model.posts))
    ]

renderPost : Post -> Html Msg
renderPost post = Html.li [] [text "foobared"]

getAllPosts : Cmd Msg
getAllPosts =
  let
    request =
      { verb = "get"
      , url = "/blog/"
      , headers = [("Accept", "application/json")]
      , body = Http.empty
      }
    task = Http.send Http.defaultSettings request
  in
    Task.perform FetchFail GotPosts (Http.fromJson decodeAllPosts task)

getOnePost : String -> Cmd Msg
getOnePost url =
  let
    request =
      { verb = "get"
      , url = "/blog/post/" ++ url
      , headers = [("Accept", "application/json")]
      , body = Http.empty
    }
    task = Http.send Http.defaultSettings request
  in
     Task.perform FetchFail GotPost (Http.fromJson decodeOnePost task)

decodeAllPosts : Json.Decoder (List Post)
decodeAllPosts = Json.at ["posts"] (Json.list decodePost)

decodeOnePost : Json.Decoder Post
decodeOnePost = at ["post"] decodePost

decodePost : Json.Decoder Post
decodePost = Json.object5 Post
    ("slug" := string)
    ("uuid" := string)
    ("title" := string)
    ("html" := string)
    ("markdown" := string)
