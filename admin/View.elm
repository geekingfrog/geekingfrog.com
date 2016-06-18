module View exposing (..)

import String as String
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as A
import ISO8601 as ISO

import Types as Types


view model =
  div []
    [ button [ onClick Types.FetchPosts ] [ text "fetch all posts" ]
    , button [ onClick (Types.FetchPost "struggles-with-parsing-json-with-aeson")] [ text "fetch one post" ]
    , div [A.class "post-list"] [renderPostsList model.posts]
    ]

renderPostsList : Maybe(Dict String Types.Post) -> Html Types.Msg
renderPostsList model =
  case model of
    Nothing -> text "No posts fetched yet"
    Just posts ->
      let
        sortedPosts = List.sortWith sortPublishedAt (Dict.values posts)
      in
        Html.ul [] (List.map renderPostHeader sortedPosts)

renderPostHeader : Types.Post -> Html Types.Msg
renderPostHeader post =
  Html.li [A.class "post-list--header"] [
      text (Maybe.withDefault "DRAFT" (lift prettyDate post.publishedAt))
    , text "  --  "
    , text post.title
    ]

prettyDate : ISO.Time -> String
prettyDate time = String.join "-" (List.map toString [time.year, time.month, time.day])

renderPost : Types.Post -> Html Types.Msg
renderPost post = Html.li [] [text (toString post.publishedAt)]

-- DRAFT firsts, then most recent to most ancient post
sortPublishedAt : Types.Post -> Types.Post -> Order
sortPublishedAt a b = case (a.publishedAt, b.publishedAt) of
  (Nothing, Nothing) -> EQ
  (Nothing, _) -> LT
  (_, Nothing) -> GT
  (Just ja, Just jb) -> compare (ISO.toTime jb) (ISO.toTime ja)


lift : (a -> b) -> Maybe a -> Maybe b
lift func thing = case thing of
  Nothing -> Nothing
  Just a -> Just (func a)
