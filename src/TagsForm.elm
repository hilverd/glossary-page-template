module TagsForm exposing (TagsForm, create, hasValidationErrors, tagsWithDescriptionsFields)

import Array exposing (Array)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)
import TagsForm.TagDescriptionField as TagDescriptionField exposing (TagDescriptionField)
import TagsForm.TagField as TagField exposing (TagField)


type TagsForm
    = TagsForm
        { tagsWithDescriptionsFields : Array ( TagField, TagDescriptionField )
        }


tagsWithDescriptionsFields : TagsForm -> Array ( TagField, TagDescriptionField )
tagsWithDescriptionsFields tagsForm =
    case tagsForm of
        TagsForm form ->
            form.tagsWithDescriptionsFields


create : List ( Tag, TagDescription ) -> TagsForm
create tags_ =
    TagsForm
        { tagsWithDescriptionsFields =
            tags_
                |> List.map
                    (\( tag, tagDescription ) ->
                        ( tag
                            |> Tag.raw
                            |> TagField.fromString
                        , tagDescription
                            |> TagDescription.raw
                            |> TagDescriptionField.fromString
                        )
                    )
                |> Array.fromList
        }


hasValidationErrors : TagsForm -> Bool
hasValidationErrors tagsForm =
    False
