module Components.AboutSection exposing (view)

import Accessibility exposing (..)
import Accessibility.Key
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection
import ElementIds
import Html.Attributes exposing (href, id, target)


view : Bool -> Data.AboutSection.AboutSection -> Html msg
view modalDialogShown aboutSection =
    case aboutSection of
        Data.AboutSection.PlaintextAboutSection { paragraph, links } ->
            div
                [ id ElementIds.about ]
                [ p []
                    [ text <| AboutParagraph.toString paragraph ]
                , ul [] <|
                    List.map
                        (\aboutLink ->
                            li []
                                [ a
                                    [ target "_blank"
                                    , href <| AboutLink.href aboutLink
                                    , Accessibility.Key.tabbable <| not modalDialogShown
                                    ]
                                    [ text <| AboutLink.body aboutLink ]
                                ]
                        )
                        links
                ]
