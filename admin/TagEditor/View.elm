module TagEditor.View exposing(..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as A

import Types exposing (Tag)
import TagEditor.Types as TagTypes

editTagsView : List Tag -> Html TagTypes.Msg
editTagsView tags =
  text "editing tags here"
