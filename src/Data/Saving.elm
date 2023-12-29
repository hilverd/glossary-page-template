module Data.Saving exposing (Saving(..))


type Saving
    = NotCurrentlySaving
    | SavingNotAttempted String
    | SavingInProgress
    | SavingFailed String
