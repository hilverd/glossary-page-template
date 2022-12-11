module TitleAndAboutForm exposing
    ( AboutLinkBody
    , AboutLinkField
    , AboutLinkHref
    , AboutParagraphField
    , TitleAndAboutForm
    , TitleField
    , aboutLinkFields
    , aboutParagraphField
    , addAboutLink
    , create
    , deleteAboutLink
    , hasValidationErrors
    , titleField
    , updateAboutLinkBody
    , updateAboutLinkHref
    , updateAboutParagraph
    , updateTitle
    )

import Array exposing (Array)
import Data.AboutLink as AboutLink
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection(..))
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Extras.Array


type alias TitleField =
    { body : String
    , validationError : Maybe String
    }


type alias AboutParagraphField =
    { body : String
    , validationError : Maybe String
    }


type alias AboutLinkHref =
    { href : String
    , validationError : Maybe String
    }


type alias AboutLinkBody =
    { body : String
    , validationError : Maybe String
    }


type alias AboutLinkField =
    ( AboutLinkHref, AboutLinkBody )


type TitleAndAboutForm
    = TitleAndAboutForm
        { titleField : TitleField
        , aboutParagraphField : AboutParagraphField
        , aboutLinkFields : Array AboutLinkField
        }


titleField : TitleAndAboutForm -> TitleField
titleField titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            form.titleField


aboutParagraphField : TitleAndAboutForm -> AboutParagraphField
aboutParagraphField titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            form.aboutParagraphField


aboutLinkFields : TitleAndAboutForm -> Array AboutLinkField
aboutLinkFields titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            form.aboutLinkFields


validate : TitleAndAboutForm -> TitleAndAboutForm
validate form =
    let
        cannotBeEmptyMessage =
            "This field can't be empty"

        titleField0 =
            titleField form

        aboutParagraphField0 =
            aboutParagraphField form

        aboutLinkFields0 =
            aboutLinkFields form

        validatedTitleField =
            { titleField0
                | validationError =
                    if titleField0 |> .body |> String.trim |> String.isEmpty then
                        Just cannotBeEmptyMessage

                    else
                        Nothing
            }

        validatedAboutParagraphField =
            { aboutParagraphField0
                | validationError =
                    if
                        aboutParagraphField0
                            |> .body
                            |> String.trim
                            |> String.isEmpty
                    then
                        Just cannotBeEmptyMessage

                    else
                        Nothing
            }

        validatedAboutLinkFields =
            aboutLinkFields0
                |> Array.map
                    (\( aboutLinkHref, aboutLinkBody ) ->
                        ( { aboutLinkHref
                            | validationError =
                                if aboutLinkHref.href |> String.trim |> String.isEmpty then
                                    Just cannotBeEmptyMessage

                                else
                                    Nothing
                          }
                        , { aboutLinkBody
                            | validationError =
                                if aboutLinkBody.body |> String.trim |> String.isEmpty then
                                    Just cannotBeEmptyMessage

                                else
                                    Nothing
                          }
                        )
                    )
    in
    TitleAndAboutForm
        { titleField = validatedTitleField
        , aboutParagraphField = validatedAboutParagraphField
        , aboutLinkFields = validatedAboutLinkFields
        }


create : GlossaryTitle -> AboutSection -> TitleAndAboutForm
create title aboutSection =
    let
        ( aboutParagraph, aboutLinks ) =
            case aboutSection of
                PlaintextAboutSection { paragraph, links } ->
                    ( paragraph, links )
    in
    TitleAndAboutForm
        { titleField = { body = GlossaryTitle.toString title, validationError = Nothing }
        , aboutParagraphField = { body = AboutParagraph.toString aboutParagraph, validationError = Nothing }
        , aboutLinkFields =
            aboutLinks
                |> List.map
                    (\aboutLink ->
                        ( { href = AboutLink.href aboutLink, validationError = Nothing }
                        , { body = AboutLink.body aboutLink, validationError = Nothing }
                        )
                    )
                |> Array.fromList
        }
        |> validate


hasValidationErrors : TitleAndAboutForm -> Bool
hasValidationErrors form =
    ((form |> titleField |> .validationError) /= Nothing)
        || ((form |> aboutParagraphField |> .validationError) /= Nothing)
        || (form
                |> aboutLinkFields
                |> Array.toList
                |> List.any
                    (\( href, body ) ->
                        (href.validationError /= Nothing)
                            || (body.validationError /= Nothing)
                    )
           )


updateTitle : String -> TitleAndAboutForm -> TitleAndAboutForm
updateTitle title titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            let
                titleField0 =
                    form.titleField
            in
            TitleAndAboutForm { form | titleField = { titleField0 | body = title } }
                |> validate


updateAboutParagraph : String -> TitleAndAboutForm -> TitleAndAboutForm
updateAboutParagraph body titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            let
                aboutParagraphField0 =
                    form.aboutParagraphField
            in
            TitleAndAboutForm
                { form
                    | aboutParagraphField =
                        { aboutParagraphField0 | body = body }
                }
                |> validate


updateAboutLinkHref : AboutLinkIndex -> String -> TitleAndAboutForm -> TitleAndAboutForm
updateAboutLinkHref index href titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            TitleAndAboutForm
                { form
                    | aboutLinkFields =
                        Extras.Array.update
                            (Tuple.mapFirst (\href0 -> { href0 | href = href }))
                            (AboutLinkIndex.toInt index)
                            form.aboutLinkFields
                }
                |> validate


updateAboutLinkBody : AboutLinkIndex -> String -> TitleAndAboutForm -> TitleAndAboutForm
updateAboutLinkBody index body titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            TitleAndAboutForm
                { form
                    | aboutLinkFields =
                        Extras.Array.update
                            (Tuple.mapSecond (\body0 -> { body0 | body = body }))
                            (AboutLinkIndex.toInt index)
                            form.aboutLinkFields
                }
                |> validate


emptyAboutLinkField : AboutLinkField
emptyAboutLinkField =
    ( { href = "", validationError = Nothing }
    , { body = "", validationError = Nothing }
    )


addAboutLink : TitleAndAboutForm -> TitleAndAboutForm
addAboutLink titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            TitleAndAboutForm
                { form | aboutLinkFields = Array.push emptyAboutLinkField form.aboutLinkFields }
                |> validate


deleteAboutLink : AboutLinkIndex -> TitleAndAboutForm -> TitleAndAboutForm
deleteAboutLink aboutLinkIndex titleAndAboutForm =
    case titleAndAboutForm of
        TitleAndAboutForm form ->
            TitleAndAboutForm
                { form | aboutLinkFields = Extras.Array.delete (AboutLinkIndex.toInt aboutLinkIndex) form.aboutLinkFields }
                |> validate
