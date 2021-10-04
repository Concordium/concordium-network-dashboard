module Paging exposing (..)

{-| Paging component, which holds state and filters the list of items and returns
the paging navigation.
-}

import Element exposing (..)
import Element.Events exposing (onClick)
import Html.Attributes


type alias Model =
    { pageIndex : Int
    , pageSize : Int
    }


type Msg
    = NextPage
    | PreviousPage
    | FirstPage


{-| Initialise paging model given a page size
-}
init : Int -> Model
init pageSize =
    { pageIndex = 0
    , pageSize = pageSize
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NextPage ->
            { model | pageIndex = model.pageIndex + 1 }

        PreviousPage ->
            { model | pageIndex = model.pageIndex - 1 }

        FirstPage ->
            { model | pageIndex = 0 }


{-| Takes the paging state and the length of items to filter, and returns the view for navigation.
Will hide the navigation if paging is not needed
-}
pager : Model -> Int -> Element Msg
pager model numberOfItems =
    let
        totalPages =
            numberOfItems // model.pageSize + 1

        from =
            model.pageIndex * model.pageSize

        to =
            min (from + model.pageSize) numberOfItems
    in
    if model.pageSize < numberOfItems then
        row []
            [ el
                [ onClick PreviousPage
                , pointer
                , visible (model.pageIndex > 0)
                ]
                (text "Prev ")
            , text <| String.fromInt (from + 1) ++ " - " ++ String.fromInt to
            , el
                [ onClick NextPage
                , pointer
                , visible (model.pageIndex < (totalPages - 1))
                ]
                (text " Next")
            ]

    else
        Element.none


{-| Takes the paging state and a list of items to filter, and returns the
filtered items.
-}
visibleItems : Model -> List a -> List a
visibleItems model items =
    let
        from =
            model.pageIndex * model.pageSize
    in
    items
        |> List.drop from
        |> List.take model.pageSize


visible : Bool -> Attribute msg
visible b =
    htmlAttribute <|
        Html.Attributes.style "visibility" <|
            if b then
                "visible"

            else
                "hidden"
