module TagEditor.Main exposing (Model, View)

import Types as Types
import TagEditor.View as View

type alias Model =
  { tags : List Types.Tag
  }
