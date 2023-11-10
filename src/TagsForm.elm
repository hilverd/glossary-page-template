module TagsForm exposing (TagsForm, create, hasValidationErrors, tagsWithDescriptionsFields, updateTag, updateTagDescription, validate)

import Array exposing (Array)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Extras.Array
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
    tagsForm
        |> tagsWithDescriptionsFields
        |> Array.toList
        |> List.any
            (\( tagField, tagDescriptionField ) ->
                TagField.validationError tagField
                    /= Nothing
                    || TagDescriptionField.validationError tagDescriptionField
                    /= Nothing
            )


validate : TagsForm -> TagsForm
validate tagsForm =
    case tagsForm of
        TagsForm form ->
            let
                cannotBeEmptyMessage : String
                cannotBeEmptyMessage =
                    "This field can't be empty"

                validateTagField : TagField -> TagField
                validateTagField tagField =
                    let
                        body : String
                        body =
                            TagField.raw tagField
                    in
                    tagField
                        |> TagField.setValidationError
                            (if String.isEmpty body then
                                Just cannotBeEmptyMessage

                             else
                                Nothing
                            )

                validateTagDescriptionField : TagDescriptionField -> TagDescriptionField
                validateTagDescriptionField tagDescriptionField =
                    let
                        body : String
                        body =
                            TagDescriptionField.raw tagDescriptionField
                    in
                    tagDescriptionField
                        |> TagDescriptionField.setValidationError
                            (if String.isEmpty body then
                                Just cannotBeEmptyMessage

                             else
                                Nothing
                            )

                validatedTagsWithDescriptionsFields : Array ( TagField, TagDescriptionField )
                validatedTagsWithDescriptionsFields =
                    form.tagsWithDescriptionsFields
                        |> Array.map
                            (\( tagField, tagDescriptionField ) ->
                                ( validateTagField tagField, validateTagDescriptionField tagDescriptionField )
                            )
            in
            TagsForm
                { tagsWithDescriptionsFields = validatedTagsWithDescriptionsFields }


updateTag : Int -> TagsForm -> String -> TagsForm
updateTag index tagsForm body =
    case tagsForm of
        TagsForm form ->
            let
                tagsWithDescriptionsFields_ =
                    Extras.Array.update
                        (\( tagField, tagDescriptionField ) ->
                            ( TagField.setBody body tagField, tagDescriptionField )
                        )
                        index
                        form.tagsWithDescriptionsFields
            in
            TagsForm { form | tagsWithDescriptionsFields = tagsWithDescriptionsFields_ }


updateTagDescription : Int -> TagsForm -> String -> TagsForm
updateTagDescription index tagsForm body =
    case tagsForm of
        TagsForm form ->
            let
                tagsWithDescriptionsFields_ =
                    Extras.Array.update
                        (\( tagField, tagDescriptionField ) ->
                            ( tagField, TagDescriptionField.setBody body tagDescriptionField )
                        )
                        index
                        form.tagsWithDescriptionsFields
            in
            TagsForm { form | tagsWithDescriptionsFields = tagsWithDescriptionsFields_ }
