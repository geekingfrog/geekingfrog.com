module State exposing (..)

import Types exposing (..)

import List as List
import Dict as Dict
import Debug as Debug
import Api as Api
import Port as Port

init : (Model, Cmd Msg)
init = (
    { posts = Nothing
    , selectedPost = Nothing
    , activeView = Index
    },
    Api.getAllPosts)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchPosts ->
      (model, Api.getAllPosts)
    FetchFail err ->
      Debug.log (toString err) (model, Cmd.none)
    FetchPost postSlug ->
      (model, Api.getOnePost postSlug)
    GotPosts posts ->
      let
        modelPosts = Dict.fromList (List.map (\p -> (p.slug, p)) posts)
      in
        ({model | posts = Just modelPosts , selectedPost = Nothing} , Cmd.none)
    GotPost post ->
      ({model | posts = Nothing}, Cmd.none)
      -- case model.posts of
      --   Nothing -> ({posts = Just (Dict.singleton post.slug post)}, Cmd.none)
      --   Just posts -> ({model | posts = Just (Dict.insert post.slug post posts)}, Cmd.none)
    SelectPost post ->
      let
        _ = Debug.log "selecting post " post.title
      in
        ({model | selectedPost = Just post, activeView = Index}, Port.highlight ())
    EditPost post ->
      ({model | selectedPost = Just post, activeView = Edit}, Port.highlight ())
    InputEditPost updatedMarkdown ->
        case (model.selectedPost, model.posts) of
          (Nothing, _) -> (model, Cmd.none)
          (_, Nothing) -> (model, Cmd.none)
          (Just p, Just ps) ->
            let
              updatedSelectedPost = Just ({p | markdown=updatedMarkdown})
              updatedPosts = Maybe.map (\p -> {p | markdown=updatedMarkdown}) (Dict.get p.slug ps)
            in
              case updatedPosts of
                Nothing -> (model, Cmd.none)
                Just p' -> (
                  { model | posts = Just (Dict.insert p'.slug p' ps)
                  , selectedPost = updatedSelectedPost
                  }, Cmd.none)

headMay : List a -> Maybe a
headMay l =
  if List.length l == 0
  then Nothing
  else List.head l
