module Data.Editability exposing
    ( Editability(..)
    , create, startEditing
    , canEdit, editing
    )

{-| Represents the ways in which this glossary page can be edited (or is being edited).


# Editability

@docs Editability


# Build

@docs create, startEditing


# Query

@docs canEdit, editing

-}


{-| The ways in which this glossary page can be edited (or is being edited).
-}
type Editability
    = ReadOnly
    | ReadOnlyWithHelpForMakingChanges
    | CanEditInMemory
    | EditingInMemory
    | CanEditWithIncludedBackend
    | EditingWithIncludedBackend
    | CanEditWithSeparateBackend String
    | EditingWithSeparateBackend String


{-| Create an Editability from flags expressing the current situation.
-}
create :
    { enableHelpForMakingChanges : Bool
    , enableSavingChangesInMemory : Bool
    , separateBackendBaseUrl : Maybe String
    , editorIsRunning : Bool
    , currentlyEditing : Bool
    }
    -> Editability
create { enableHelpForMakingChanges, enableSavingChangesInMemory, separateBackendBaseUrl, editorIsRunning, currentlyEditing } =
    case ( ( enableHelpForMakingChanges, enableSavingChangesInMemory ), ( separateBackendBaseUrl, editorIsRunning ) ) of
        ( ( False, False ), ( _, False ) ) ->
            ReadOnly

        ( ( False, False ), ( Nothing, True ) ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend

        ( ( False, False ), ( Just separateBackendBaseUrl_, True ) ) ->
            if currentlyEditing then
                EditingWithSeparateBackend separateBackendBaseUrl_

            else
                CanEditWithSeparateBackend separateBackendBaseUrl_

        ( ( False, True ), ( _, False ) ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( ( False, True ), ( _, True ) ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( ( True, False ), ( _, False ) ) ->
            ReadOnlyWithHelpForMakingChanges

        ( ( True, False ), ( Nothing, True ) ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend

        ( ( True, False ), ( Just separateBackendBaseUrl_, True ) ) ->
            if currentlyEditing then
                EditingWithSeparateBackend separateBackendBaseUrl_

            else
                CanEditWithSeparateBackend separateBackendBaseUrl_

        ( ( True, True ), ( _, False ) ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( ( True, True ), ( Nothing, True ) ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend

        ( ( True, True ), ( Just separateBackendBaseUrl_, True ) ) ->
            if currentlyEditing then
                EditingWithSeparateBackend separateBackendBaseUrl_

            else
                CanEditWithSeparateBackend separateBackendBaseUrl_


{-| Begin editing a glossary (that can be edited).
-}
startEditing : Editability -> Editability
startEditing editability =
    case editability of
        CanEditInMemory ->
            EditingInMemory

        CanEditWithIncludedBackend ->
            EditingWithIncludedBackend

        CanEditWithSeparateBackend baseUrl ->
            EditingWithSeparateBackend baseUrl

        _ ->
            editability


{-| Whether or not the glossary is currently being edited.
-}
editing : Editability -> Bool
editing editability =
    case editability of
        EditingInMemory ->
            True

        EditingWithIncludedBackend ->
            True

        EditingWithSeparateBackend _ ->
            True

        _ ->
            False


{-| Whether or not the glossary can be edited.
-}
canEdit : Editability -> Bool
canEdit editability =
    case editability of
        CanEditInMemory ->
            True

        CanEditWithIncludedBackend ->
            True

        CanEditWithSeparateBackend _ ->
            True

        _ ->
            False
