module Main exposing (..)

import Html.App as Html
import Types as Types exposing (Model, Msg)
import State exposing (init, update)
import View exposing (view)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }
