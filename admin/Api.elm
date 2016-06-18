module Api exposing (..)

import Http as Http
import Task as Task

import Types as Types
import Parse exposing
  ( decodeAllPosts
  , decodeOnePost
  )


getAllPosts : Cmd Types.Msg
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
    Task.perform Types.FetchFail Types.GotPosts (Http.fromJson decodeAllPosts task)

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
