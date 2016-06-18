module State exposing (..)

import Types exposing (..)

import Dict as Dict
import Debug as Debug
import Api as Api


init : (Model, Cmd Msg)
init = ({ posts = Nothing }, Api.getAllPosts)


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
        ({posts = Just modelPosts}, Cmd.none)
    GotPost post ->
      case model.posts of
        Nothing -> ({posts = Just (Dict.singleton post.slug post)}, Cmd.none)
        Just posts -> ({model | posts = Just (Dict.insert post.slug post posts)}, Cmd.none)
