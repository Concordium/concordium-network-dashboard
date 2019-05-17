module Widgets exposing (activeNavLink, border_, buttonExternal, buttonInternal, centerTextWhenSmall, centerWhenSmall, h3, h3black, h4, markdown, p_, padding_, responsiveGrid, rounded_, rowToColumnWhenSmall, spinner, standardDropShadow, theme)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import List.Extra as List
import Markdown
import Types exposing (..)
import WebsiteColors exposing (..)


centerTextWhenSmall model =
    if model.window.width < 600 then
        Font.center

    else
        alignLeft


centerWhenSmall model =
    if model.window.width < 600 then
        centerX

    else
        alignLeft


rowToColumnWhenSmall model attrs elems =
    if model.window.width < 600 then
        column attrs elems

    else
        row attrs elems


responsiveGrid model attrs large medium small elems =
    let
        grouping =
            if model.window.width > 950 then
                large

            else if model.window.width > 700 then
                medium

            else
                small
    in
    elems
        |> List.greedyGroupsOf grouping
        |> List.map (row [ spaceEvenly, width fill ])
        |> column attrs


activeNavLink model page path linkText imageUrl =
    let
        element =
            paragraph [ spacing 10, Font.center ]
                [ text linkText
                ]
    in
    if model.currentPage == page then
        link [ width (fillPortion 1), Font.center, centerX, Background.color navy, paddingXY 20 10, Border.rounded 3 ] { url = path, label = element }

    else
        link [ width (fillPortion 1), Font.center, centerX, paddingXY 20 10, Border.rounded 3 ] { url = path, label = element }


buttonInternal page attrs text_ =
    link attrs
        { url = pageToPath page
        , label =
            Input.button [ Background.color navy, padding 20, Font.center, Font.color white, Border.rounded 50 ]
                { onPress = Nothing
                , label = text text_
                }
        }


buttonExternal url attrs text_ =
    newTabLink (attrs ++ [ htmlAttribute <| Html.Attributes.attribute "rel" "nofollow" ])
        { url = url
        , label =
            Input.button [ Background.color navy, padding 20, Font.center, Font.color white, Border.rounded 50 ]
                { onPress = Nothing
                , label = text text_
                }
        }


theme : List (Element msg) -> Html.Html msg
theme x =
    layout
        [ width fill
        , Background.color darkGrey
        , Font.color white
        , Font.size 15
        , Font.family
            [ Font.typeface "Montserrat"
            , Font.sansSerif
            ]
        ]
        (column [ width fill, spacing 20 ]
            x
        )


padding_ l t r b =
    paddingEach { bottom = b, left = l, right = r, top = t }


border_ l t r b =
    Border.widthEach { bottom = b, left = l, right = r, top = t }


rounded_ l t r b =
    Border.roundEach { topLeft = l, topRight = t, bottomRight = r, bottomLeft = b }


p_ attrs elems =
    paragraph (attrs ++ [ spacing 10 ]) elems


h3 title =
    row [ Font.size 25, spacing 20 ] [ p_ [ Font.bold, Font.color navy ] [ text title ] ]


h3black title =
    row [ Font.size 25, spacing 20 ] [ p_ [ Font.bold, Font.color black ] [ text title ] ]


h4 attrs title =
    row ([ Font.size 16, spacing 20 ] ++ attrs) [ p_ [ Font.bold, Font.color black ] [ text title ] ]


markdown : String -> Element msg
markdown string =
    Element.html <| Markdown.toHtml [] string


spinner =
    el
        [ width (px 20)
        , height (px 20)
        , htmlAttribute (Html.Attributes.class "spin")
        , htmlAttribute
            (Html.Attributes.style "border-top-color" "#FFF")
        , Border.solid
        , Border.width 3
        , Border.rounded 50
        ]
        none


standardDropShadow =
    Border.shadow { offset = ( 0, 3 ), size = 0, blur = 20, color = Element.rgba255 0 0 0 0.1 }
