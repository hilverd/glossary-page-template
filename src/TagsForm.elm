module TagsForm exposing (TagsForm, create, tags)

import Array exposing (Array)
import Data.GlossaryItem.Tag exposing (Tag)


type TagsForm
    = TagsForm
        { tags : Array Tag
        }


tags : TagsForm -> Array Tag
tags tagsForm =
    case tagsForm of
        TagsForm form ->
            form.tags


create : List Tag -> TagsForm
create tags_ =
    TagsForm
        { tags = Array.fromList tags_ }
