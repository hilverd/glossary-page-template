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


{-| Create an Editability from flags expressing the current situation.
-}
create :
    { enableHelpForMakingChanges : Bool
    , enableSavingChangesInMemory : Bool
    , editorIsRunning : Bool
    , currentlyEditing : Bool
    }
    -> Editability
create { enableHelpForMakingChanges, enableSavingChangesInMemory, editorIsRunning, currentlyEditing } =
    case ( enableHelpForMakingChanges, enableSavingChangesInMemory, editorIsRunning ) of
        ( False, False, False ) ->
            ReadOnly

        ( False, False, True ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend

        ( False, True, False ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( False, True, True ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( True, False, False ) ->
            ReadOnlyWithHelpForMakingChanges

        ( True, False, True ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend

        ( True, True, False ) ->
            if currentlyEditing then
                EditingInMemory

            else
                CanEditInMemory

        ( True, True, True ) ->
            if currentlyEditing then
                EditingWithIncludedBackend

            else
                CanEditWithIncludedBackend


{-| Begin editing a glossary (that can be edited).
-}
startEditing : Editability -> Editability
startEditing editability =
    case editability of
        CanEditInMemory ->
            EditingInMemory

        CanEditWithIncludedBackend ->
            EditingWithIncludedBackend

        _ ->
            editability


{-| Whether or not the glossary page is currently being edited.
-}
editing : Editability -> Bool
editing editability =
    case editability of
        EditingInMemory ->
            True

        EditingWithIncludedBackend ->
            True

        _ ->
            False


{-| Whether or not the glossary page can be edited.
-}
canEdit : Editability -> Bool
canEdit editability =
    case editability of
        CanEditInMemory ->
            True

        CanEditWithIncludedBackend ->
            True

        _ ->
            False
