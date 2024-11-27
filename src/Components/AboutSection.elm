module Components.AboutSection exposing (view)

import Accessibility exposing (Html, a, div, li, text, ul)
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection
import ElementIds
import Extras.Html
import Html.Attributes exposing (href, id, target)


view :
    { enableMathSupport : Bool }
    -> Data.AboutSection.AboutSection
    -> Html msg
view { enableMathSupport } { paragraph, links } =
    div
        [ id ElementIds.about ]
        [ div []
            [ AboutParagraph.view { enableMathSupport = enableMathSupport } paragraph ]
        , Extras.Html.showIf (not <| List.isEmpty links) <|
            ul [] <|
                List.map
                    (\aboutLink ->
                        li []
                            [ a
                                [ target "_blank"
                                , href <| AboutLink.href aboutLink
                                ]
                                [ text <| AboutLink.body aboutLink ]
                            ]
                    )
                    links
        ]
