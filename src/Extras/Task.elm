module Extras.Task exposing (messageToCommand)

import Task


messageToCommand : msg -> Cmd msg
messageToCommand =
    Task.succeed >> Task.perform identity
