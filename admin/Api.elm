module Api exposing (..)

import Http as Http
import Task as Task

import Types as Types
import Parse exposing
  ( decodeAllPosts
  , decodeOnePost
  , decodeAllTags
  )

getInitialData : Cmd Types.Msg
getInitialData =
  let
    requestPosts = getAllPosts
    requestTags = getAllTags
    combined = Task.map2 (\posts tags -> (posts, tags)) requestPosts requestTags
  in
    Task.perform Types.FetchFail Types.GotInitialData combined


-- getAllPosts : Cmd Types.Msg
getAllPosts : Task.Task Http.Error (List Types.Post)
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
    Http.fromJson decodeAllPosts task
    -- Task.perform Types.FetchFail Types.GotPosts (Http.fromJson decodeAllPosts task)

getOnePost : String -> Cmd Types.Msg
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
     Task.perform Types.FetchFail Types.GotPost (Http.fromJson decodeOnePost task)

getAllTags : Task.Task Http.Error (List Types.Tag)
getAllTags =
  let
    request =
      { verb = "get"
      , url = "/api/tag/"
      , headers = [("Accept", "application/json")]
      , body = Http.empty
      }
    task = Http.send Http.defaultSettings request
  in
    Http.fromJson decodeAllTags task
