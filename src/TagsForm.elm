module TagsForm exposing (TagsForm)

import Array exposing (Array)
import Data.GlossaryItem.Tag exposing (Tag)


type alias TagField =
    { tag : Tag }


type TagsForm
    = TagsForm
        { tags : Array Tag
        }
