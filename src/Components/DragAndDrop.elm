module Components.DragAndDrop exposing
    ( Model, init, Msg, Position, update, updateSticky
    , draggable, droppable
    , getDragId, getDropId, getDroppablePosition
    , getDragstartEvent
    )

{-| This module is adapted from <https://github.com/norpan/elm-html5-drag-drop>.

--- Start of original copyright notice ---

Copyright 2018 Martin Norbäck Olivers

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1.  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2.  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.  Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--- End of original copyright notice ---

This library handles dragging and dropping using the API
from the HTML 5 recommendation at
<https://www.w3.org/TR/html/editing.html#drag-and-drop>.

It provides attributes and a model/update to handle
dragging and dropping between your elements.

Types are parametrized with a `dragId` and a `dropId` parameter, which are the
types for the drag identifier passed to the [`draggable`](#draggable) function
and the drop identifier passed to the [`droppable`](#droppable) function.
You can put whatever data you like in these, but don't use function types.

You can use several instances of this model at the same time and they won't
interfere with each other. Drag and drop are connected to an instance by the
Msg constructor used, and the update function will not send a result if a drop
was made from another instance.

To use on mobile, you can include the following polyfill:
<https://github.com/Bernardo-Castilho/dragdroptouch>

Note that drag and drop _does not_ work out of the box in Firefox.
See the example folder in github for an example that uses ports
to do `event.dataTransfer.setData('text', '')`. to fix this.


# Model and update

@docs Model, init, Msg, Position, update, updateSticky


# View attributes

@docs draggable, droppable


# Status functions

@docs getDragId, getDropId, getDroppablePosition


# Javascript interop

@docs getDragstartEvent

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


{-| The drag and drop state.

This should be placed inside your application's model like this:

    type alias Model =
        { ...
        , dragAndDrop : Components.DragAndDrop.Model DragId DropId
        }

-}
type Model dragId dropId
    = NotDragging
    | Dragging dragId
    | DraggedOver dragId dropId Int (Maybe Position)


{-| The position inside a droppable. Contains the droppable's
width and height, as well as the current x and y position,
using the `currentTarget.clientWidth`, `currentTarget.clientHeight`, `offsetX`, and `offsetY`
from the `ondragover` event.

Note, that in some cases, x and y may be negative, or larger than the clientWidth and height,
if a drop event is registered outside the CSS padding edge.

-}
type alias Position =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    }


{-| The initial drag and drop state.

You should use this as the initital value for the drag and drop state in your model.

-}
init : Model dragId dropId
init =
    NotDragging


{-| The drag and drop messages.

This should be placed inside your application's messages like this:

    type Msg
        = ...
        | DragAndDropMsg (Components.DragAndDrop.Msg DragId DropId)

-}
type Msg dragId dropId
    = DragStart dragId Json.Value
    | DragEnd
    | DragEnter dropId
    | DragLeave dropId
    | DragOver dropId Int Position
    | Drop dropId Position


{-| The update function.

When a successful drag and drop is made, this function will return a result
consisting of the `dragId` and `dropId` that was specified in the
[`draggable`](#draggable) and [`droppable`](#droppable)
calls for the corresponding nodes. It will also return a [`Position`](#Position)
for the drop event.

This should be placed inside your application's update function, like this:

    update msg model =
        case msg of
            ...
            DragAndDropMsg msg_ ->
                let
                    ( model_, result ) =
                        Components.DragAndDrop.update msg_ model.dragAndDrop
                in
                    { model
                        | dragAndDrop = model_
                        , ...use result if available...
                    }

-}
update : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe ( dragId, dropId, Position ) )
update =
    updateCommon False


{-| A "sticky" version of the [`update`](#update) function.

It's used the same way as the [`update`](#update) function, but when you use this version,
droppables are "sticky" so when you drag out of them and release the mouse button,
a drop will still be registered at the last droppable. You should preferably
provide some sort of indication (using [`getDropId`](#getDropId)) where the drop will take
place if you use this function.

-}
updateSticky : Msg dragId dropId -> Model dragId dropId -> ( Model dragId dropId, Maybe ( dragId, dropId, Position ) )
updateSticky =
    updateCommon True


updateCommon :
    Bool
    -> Msg dragId dropId
    -> Model dragId dropId
    -> ( Model dragId dropId, Maybe ( dragId, dropId, Position ) )
updateCommon sticky msg model =
    case ( msg, model, sticky ) of
        ( DragStart dragId _, _, _ ) ->
            ( Dragging dragId, Nothing )

        ( DragEnd, _, _ ) ->
            ( NotDragging, Nothing )

        ( DragEnter dropId, Dragging dragId, _ ) ->
            ( DraggedOver dragId dropId 0 Nothing, Nothing )

        ( DragEnter dropId, DraggedOver dragId _ _ pos, _ ) ->
            ( DraggedOver dragId dropId 0 pos, Nothing )

        -- Only handle DragLeave if it is for the current dropId.
        -- DragLeave and DragEnter sometimes come in the wrong order
        -- when two droppables are next to each other.
        ( DragLeave dropId_, DraggedOver dragId dropId _ _, False ) ->
            if dropId_ == dropId then
                ( Dragging dragId, Nothing )

            else
                ( model, Nothing )

        ( DragOver dropId timeStamp pos, Dragging dragId, _ ) ->
            ( DraggedOver dragId dropId timeStamp (Just pos), Nothing )

        ( DragOver dropId timeStamp pos, DraggedOver dragId currentDropId currentTimeStamp currentPos, _ ) ->
            if timeStamp == currentTimeStamp then
                -- Handle dragover bubbling, if we already have handled this event
                -- (by looking at the timeStamp), do nothing. Also, this does some rate limiting
                -- if multiple events occur in the same time stamp.
                ( model, Nothing )

            else
                -- Update coordinates
                ( DraggedOver dragId dropId timeStamp (Just pos), Nothing )

        ( Drop dropId pos, Dragging dragId, _ ) ->
            ( NotDragging, Just ( dragId, dropId, pos ) )

        ( Drop dropId pos, DraggedOver dragId _ _ _, _ ) ->
            ( NotDragging, Just ( dragId, dropId, pos ) )

        _ ->
            ( model, Nothing )


{-| Attributes to make a node draggable.

The node you put these attributes on will be draggable with the `dragId` you provide.
It should be used like this:

    view =
       ...
       div (... ++ Components.DragAndDrop.draggable DragAndDropMsg dragId) [...]

-}
draggable : (Msg dragId dropId -> msg) -> dragId -> List (Attribute msg)
draggable wrap drag =
    [ attribute "draggable" "true"
    , onWithOptions "dragstart" { stopPropagation = True, preventDefault = False } <| Json.map (wrap << DragStart drag) Json.value
    , onWithOptions "dragend" { stopPropagation = True, preventDefault = False } <| Json.succeed <| wrap <| DragEnd
    ]


{-| Attributes to make a node droppable.

The node you put these attributes on will be droppable with the `dropId` you provide.
It should be used like this:

    view =
       ...
       div (... ++ Components.DragAndDrop.droppable DragAndDropMsg dropId) [...]

-}
droppable : (Msg dragId dropId -> msg) -> dropId -> List (Attribute msg)
droppable wrap dropId =
    [ onWithOptions "dragenter" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| DragEnter dropId
    , onWithOptions "dragleave" { stopPropagation = True, preventDefault = True } <| Json.succeed <| wrap <| DragLeave dropId

    -- We don't stop propagation for dragover events because this will trigger redraw,
    -- and we get a lot of dragover events.
    , onWithOptions "dragover" { stopPropagation = False, preventDefault = True } <| Json.map wrap <| Json.map2 (DragOver dropId) timeStampDecoder positionDecoder
    , onWithOptions "drop" { stopPropagation = True, preventDefault = True } <| Json.map (wrap << Drop dropId) positionDecoder
    ]


timeStampDecoder : Json.Decoder Int
timeStampDecoder =
    Json.at [ "timeStamp" ] Json.float |> Json.map round


positionDecoder : Json.Decoder Position
positionDecoder =
    Json.map4 Position
        (Json.at [ "currentTarget", "clientWidth" ] Json.int)
        (Json.at [ "currentTarget", "clientHeight" ] Json.int)
        (Json.at [ "offsetX" ] Json.float |> Json.map round)
        (Json.at [ "offsetY" ] Json.float |> Json.map round)


{-| Get the current `dragId` if available.

This function can be used for instance to hide the draggable when dragging.

-}
getDragId : Model dragId dropId -> Maybe dragId
getDragId model =
    case model of
        NotDragging ->
            Nothing

        Dragging dragId ->
            Just dragId

        DraggedOver dragId dropId _ _ ->
            Just dragId


{-| Get the current `dropId` if available.

This function can be used for instance to highlight the droppable when dragging over it.

Note that for efficiency reasons, the `dragover` event is being propagated,
so if you have a droppable inside another droppable you could get the wrong info
from `getDropId`. The package tries to ignore the extra events, but it may fail.

-}
getDropId : Model dragId dropId -> Maybe dropId
getDropId model =
    case model of
        NotDragging ->
            Nothing

        Dragging dragId ->
            Nothing

        DraggedOver dragId dropId _ _ ->
            Just dropId


{-| Get the current `Position` when dragging over the droppable.
-}
getDroppablePosition : Model dragId dropId -> Maybe Position
getDroppablePosition model =
    case model of
        DraggedOver _ _ _ pos ->
            pos

        _ ->
            Nothing


{-| Get the `dragstart` event `Value` so that you can pass it to a port.
This is useful to fix Firefox behaviour. See the example directory in github
for how you can do that.

You can also use the event to do other things from Javascript,
such as setting the drag image.

-}
getDragstartEvent : Msg dragId dropId -> Maybe { dragId : dragId, event : Json.Value }
getDragstartEvent msg =
    case msg of
        DragStart dragId event ->
            Just { dragId = dragId, event = event }

        _ ->
            Nothing


{-| polyfill for onWithOptions
-}
onWithOptions :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decoder msg
    -> Attribute msg
onWithOptions name { stopPropagation, preventDefault } decoder =
    decoder
        |> Json.map (\msg -> { message = msg, stopPropagation = stopPropagation, preventDefault = preventDefault })
        |> custom name
