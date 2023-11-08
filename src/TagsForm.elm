module TagsForm exposing (TagsForm, create, hasValidationErrors, tagsWithDescriptions)

import Array exposing (Array)
import Data.GlossaryItem.Tag exposing (Tag)
import Data.TagDescription exposing (TagDescription)


type TagsForm
    = TagsForm
        { tagsWithDescriptions : Array ( Tag, TagDescription )
        }


tagsWithDescriptions : TagsForm -> Array ( Tag, TagDescription )
tagsWithDescriptions tagsForm =
    case tagsForm of
        TagsForm form ->
            form.tagsWithDescriptions


create : List ( Tag, TagDescription ) -> TagsForm
create tags_ =
    TagsForm
        { tagsWithDescriptions = Array.fromList tags_ }


hasValidationErrors : TagsForm -> Bool
hasValidationErrors tagsForm =
    False
