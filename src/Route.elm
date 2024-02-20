module Route exposing (CommonParameters, Route(..), path)

import Browser.Navigation
import Data.Editability exposing (Editability)
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItems as GlossaryItems
import Data.Theme exposing (Theme)
import QueryParameters exposing (QueryParameters)
import Url exposing (Url)
import Url.Builder as Builder


type alias CommonParameters =
    { key : Browser.Navigation.Key
    , initialUrl : Url
    , filename : Maybe String
    , theme : Theme
    , editability : Editability
    , enableMathSupport : Bool
    , glossary : Result String Glossary
    }


type Route
    = ListAll QueryParameters (Maybe GlossaryItemId) CommonParameters
    | CreateOrEdit GlossaryItemId QueryParameters CommonParameters
    | EditTitleAndAbout QueryParameters CommonParameters
    | ManageTags QueryParameters CommonParameters


path : Route -> String
path =
    urlString Builder.Absolute


urlString : Builder.Root -> Route -> String
urlString root route =
    let
        custom3 : List String -> List Builder.QueryParameter -> Maybe String -> String
        custom3 pathElements queryParams_ fragment_ =
            Builder.custom root pathElements queryParams_ fragment_

        custom2 : List String -> List Builder.QueryParameter -> String
        custom2 pathElements queryParams_ =
            Builder.custom root pathElements queryParams_ Nothing
    in
    case route of
        ListAll queryParameters maybeGlossaryItemId commonParameters ->
            let
                disambiguatedPreferredTermIdString =
                    Maybe.map2
                        (\glossaryItems glossaryItemId ->
                            GlossaryItems.get glossaryItemId glossaryItems
                                |> Maybe.map
                                    GlossaryItemForHtml.disambiguatedPreferredTermIdString
                        )
                        (commonParameters.glossary
                            |> Result.toMaybe
                            |> Maybe.map Glossary.items
                        )
                        maybeGlossaryItemId
                        |> Maybe.andThen identity
            in
            custom3 []
                (QueryParameters.toUrlQueryParams queryParameters)
                disambiguatedPreferredTermIdString

        CreateOrEdit _ queryParameters _ ->
            custom2 []
                (QueryParameters.toUrlQueryParams queryParameters)

        EditTitleAndAbout queryParameters _ ->
            custom2 []
                (QueryParameters.toUrlQueryParams queryParameters)

        ManageTags queryParameters _ ->
            custom2 []
                (QueryParameters.toUrlQueryParams queryParameters)
