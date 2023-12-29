module Data.Saving exposing (Saving(..))


type Saving
    = NotCurrentlySaving
    | SavingInProgress
    | SavingFailed String
