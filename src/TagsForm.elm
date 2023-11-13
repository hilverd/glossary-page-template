module TagsForm exposing (Row(..), TagsForm, addRow, changes, create, deleteRow, hasValidationErrors, rows, updateTag, updateTagDescription, validate)

import Array exposing (Array)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Extras.Array
import Set
import TagsForm.TagDescriptionField as TagDescriptionField exposing (TagDescriptionField)
import TagsForm.TagField as TagField exposing (TagField)


type Row
    = Existing
        { id : TagId
        , tagField : TagField
        , tagDescriptionField : TagDescriptionField
        }
    | Deleted TagId
    | New
        { tagField : TagField
        , tagDescriptionField : TagDescriptionField
        }


type TagsForm
    = TagsForm { rows : Array Row }


changes : TagsForm -> TagsChanges
changes tagsForm =
    case tagsForm of
        TagsForm form ->
            form.rows
                |> Array.foldl
                    (\row ->
                        case row of
                            Existing { id, tagField, tagDescriptionField } ->
                                TagsChanges.update id
                                    (tagField |> TagField.raw |> String.trim |> Tag.fromMarkdown)
                                    (tagDescriptionField |> TagDescriptionField.raw |> String.trim |> TagDescription.fromMarkdown)

                            Deleted tagId ->
                                TagsChanges.remove tagId

                            New { tagField, tagDescriptionField } ->
                                TagsChanges.insert
                                    (tagField |> TagField.raw |> String.trim |> Tag.fromMarkdown)
                                    (tagDescriptionField |> TagDescriptionField.raw |> String.trim |> TagDescription.fromMarkdown)
                    )
                    TagsChanges.empty


rows : TagsForm -> Array Row
rows tagsForm =
    case tagsForm of
        TagsForm form ->
            form.rows


create : List { id : TagId, tag : Tag, description : TagDescription } -> TagsForm
create rows_ =
    TagsForm
        { rows =
            rows_
                |> List.map
                    (\{ id, tag, description } ->
                        Existing
                            { id = id
                            , tagField =
                                tag
                                    |> Tag.raw
                                    |> TagField.fromString
                            , tagDescriptionField =
                                description
                                    |> TagDescription.raw
                                    |> TagDescriptionField.fromString
                            }
                    )
                |> Array.fromList
        }
        |> validate


hasValidationErrors : TagsForm -> Bool
hasValidationErrors tagsForm =
    tagsForm
        |> rows
        |> Array.toList
        |> List.any
            (\row ->
                case row of
                    Existing { tagField, tagDescriptionField } ->
                        TagField.validationError tagField
                            /= Nothing
                            || TagDescriptionField.validationError tagDescriptionField
                            /= Nothing

                    Deleted _ ->
                        False

                    New { tagField, tagDescriptionField } ->
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
                            tagField |> TagField.raw |> String.trim
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
                            TagDescriptionField.raw tagDescriptionField |> String.trim
                    in
                    tagDescriptionField
                        |> TagDescriptionField.setValidationError
                            (if String.isEmpty body then
                                Just cannotBeEmptyMessage

                             else
                                Nothing
                            )

                validateRow : Row -> Row
                validateRow row =
                    case row of
                        Existing { id, tagField, tagDescriptionField } ->
                            Existing
                                { id = id
                                , tagField = validateTagField tagField
                                , tagDescriptionField = validateTagDescriptionField tagDescriptionField
                                }

                        Deleted _ ->
                            row

                        New { tagField, tagDescriptionField } ->
                            New
                                { tagField = validateTagField tagField
                                , tagDescriptionField = validateTagDescriptionField tagDescriptionField
                                }

                duplicateTagMessage : String
                duplicateTagMessage =
                    "This tag is a duplicate of an earlier one"

                rows_ =
                    form.rows
                        |> Array.toList
                        |> List.map validateRow
                        |> List.foldl
                            (\row ( result, trimmedRawTagsSeenSoFar ) ->
                                case row of
                                    Existing record ->
                                        let
                                            trimmedRawTag =
                                                record.tagField |> TagField.raw |> String.trim

                                            validatedRow =
                                                if Set.member trimmedRawTag trimmedRawTagsSeenSoFar then
                                                    Existing
                                                        { record
                                                            | tagField =
                                                                record.tagField
                                                                    |> TagField.setValidationError (Just duplicateTagMessage)
                                                        }

                                                else
                                                    row
                                        in
                                        ( validatedRow :: result
                                        , Set.insert trimmedRawTag trimmedRawTagsSeenSoFar
                                        )

                                    Deleted _ ->
                                        ( row :: result
                                        , trimmedRawTagsSeenSoFar
                                        )

                                    New record ->
                                        let
                                            trimmedRawTag =
                                                record.tagField |> TagField.raw |> String.trim

                                            validatedRow =
                                                if Set.member trimmedRawTag trimmedRawTagsSeenSoFar then
                                                    New
                                                        { record
                                                            | tagField =
                                                                record.tagField
                                                                    |> TagField.setValidationError (Just duplicateTagMessage)
                                                        }

                                                else
                                                    row
                                        in
                                        ( validatedRow :: result
                                        , Set.insert trimmedRawTag trimmedRawTagsSeenSoFar
                                        )
                            )
                            ( [], Set.empty )
                        |> Tuple.first
                        |> List.reverse
                        |> Array.fromList
            in
            TagsForm
                { rows = rows_ }


updateTag : Int -> TagsForm -> String -> TagsForm
updateTag index tagsForm body =
    case tagsForm of
        TagsForm form ->
            let
                rows_ =
                    Extras.Array.update
                        (\row ->
                            case row of
                                Existing record ->
                                    Existing { record | tagField = TagField.setBody body record.tagField }

                                Deleted _ ->
                                    row

                                New record ->
                                    New { record | tagField = TagField.setBody body record.tagField }
                        )
                        index
                        form.rows
            in
            TagsForm { form | rows = rows_ }
                |> validate


updateTagDescription : Int -> TagsForm -> String -> TagsForm
updateTagDescription index tagsForm body =
    case tagsForm of
        TagsForm form ->
            let
                rows_ =
                    Extras.Array.update
                        (\row ->
                            case row of
                                Existing record ->
                                    Existing { record | tagDescriptionField = TagDescriptionField.setBody body record.tagDescriptionField }

                                Deleted _ ->
                                    row

                                New record ->
                                    New { record | tagDescriptionField = TagDescriptionField.setBody body record.tagDescriptionField }
                        )
                        index
                        form.rows
            in
            TagsForm { form | rows = rows_ }
                |> validate


addRow : TagsForm -> TagsForm
addRow tagsForm =
    case tagsForm of
        TagsForm form ->
            TagsForm
                { form
                    | rows =
                        Array.push
                            (New { tagField = TagField.empty, tagDescriptionField = TagDescriptionField.empty })
                            form.rows
                }
                |> validate


deleteRow : Int -> TagsForm -> TagsForm
deleteRow index tagsForm =
    case tagsForm of
        TagsForm form ->
            let
                rows_ =
                    form.rows
                        |> Array.indexedMap Tuple.pair
                        |> Array.toList
                        |> List.filterMap
                            (\( index_, row ) ->
                                if index_ == index then
                                    case row of
                                        Existing { id } ->
                                            Just <| Deleted id

                                        Deleted id ->
                                            Just <| Deleted id

                                        New _ ->
                                            Nothing

                                else
                                    Just row
                            )
                        |> Array.fromList
            in
            TagsForm { form | rows = rows_ }
                |> validate
