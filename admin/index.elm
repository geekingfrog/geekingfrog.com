module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes as A
import Http
import Json.Decode
import Json.Decode as Json exposing (..)
import Task as Task
import Debug as Debug
import Dict as Dict exposing (..)
import Maybe as Maybe
import Result as Result
import ISO8601 as ISO
import String as String

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

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

init : (Model, Cmd Msg)
init = ({ posts = Nothing }, getAllPosts)


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
      let
        modelPosts = Dict.fromList (List.map (\p -> (p.slug, p)) posts)
      in
        ({posts = Just modelPosts}, Cmd.none)
    GotPost post ->
      case model.posts of
        Nothing -> ({posts = Just (Dict.singleton post.slug post)}, Cmd.none)
        Just posts -> ({model | posts = Just (Dict.insert post.slug post posts)}, Cmd.none)


view model =
  div []
    [ button [ onClick FetchPosts ] [ text "fetch all posts" ]
    , button [ onClick (FetchPost "struggles-with-parsing-json-with-aeson")] [ text "fetch one post" ]
    , div [A.class "post-list"] [renderPostsList model.posts]
    ]


-- DRAFT firsts, then most recent to most ancient post
sortPublishedAt : Post -> Post -> Order
sortPublishedAt a b = case (a.publishedAt, b.publishedAt) of
  (Nothing, Nothing) -> EQ
  (Nothing, _) -> LT
  (_, Nothing) -> GT
  (Just ja, Just jb) -> compare (ISO.toTime jb) (ISO.toTime ja)


renderPostsList : Maybe(Dict String Post) -> Html Msg
renderPostsList model =
  case model of
    Nothing -> text "No posts fetched yet"
    Just posts ->
      let
        sortedPosts = List.sortWith sortPublishedAt (Dict.values posts)
      in
        Html.ul [] (List.map renderPostHeader sortedPosts)

renderPostHeader : Post -> Html Msg
renderPostHeader post =
  Html.li [A.class "post-list--header"] [
      text (Maybe.withDefault "DRAFT" (lift prettyDate post.publishedAt))
    , text "  --  "
    , text post.title
    ]

prettyDate : ISO.Time -> String
prettyDate time = String.join "-" (List.map toString [time.year, time.month, time.day])

renderPost : Post -> Html Msg
renderPost post = Html.li [] [text (toString post.publishedAt)]

getAllPosts : Cmd Msg
getAllPosts =
  let
    request =
      { verb = "get"
      , url = "/api/post/"
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
      , url = "/api/post/" ++ url
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
decodePost = Json.object6 Post
    ("slug" := string)
    ("uuid" := string)
    ("title" := string)
    ("html" := string)
    ("markdown" := string)
    ("publishedAt" := oneOf
      [ null Nothing
      , Json.map parseDate string
      ]
    )

    -- ("publishedAt" := (nullOr (Json.map parseDate string)))
    -- (oneOf
    -- [ null Nothing
    -- , Json.map parseDate ("publishedAt" := string)
    -- ])

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
  oneOf
  [ null Nothing
  , Json.map Just decoder
  ]

parseDate : String -> Maybe(ISO.Time)
parseDate str = Result.toMaybe (ISO.fromString str)
  -- Nothing -> Nothing
  -- Just s -> Result.toMaybe (ISO.fromString s)

lift : (a -> b) -> Maybe a -> Maybe b
lift func thing = case thing of
  Nothing -> Nothing
  Just a -> Just (func a)
