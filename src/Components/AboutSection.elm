module Components.AboutSection exposing (view)

import Accessibility exposing (Html, a, div, li, text, ul)
import Accessibility.Key
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection
import ElementIds
import Extras.Html
import Html.Attributes exposing (href, id, target)


view : Bool -> Data.AboutSection.AboutSection -> Html msg
view modalDialogShown { paragraph, links } =
    div
        [ id ElementIds.about ]
        [ div []
            [ AboutParagraph.view paragraph ]
        , Extras.Html.showIf (not <| List.isEmpty links) <|
            ul [] <|
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
