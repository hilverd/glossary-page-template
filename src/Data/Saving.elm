module Data.Saving exposing (Saving(..))


type Saving
    = NotSaving
    | SavingInProgress
    | SavingFailed String
