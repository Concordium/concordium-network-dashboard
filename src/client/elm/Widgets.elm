module Widgets exposing (..)

import Context exposing (Context)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers exposing (..)
import Icons
import Loading
import Palette exposing (Palette, toHex, veryDark, withAlphaEl)
import RemoteData exposing (RemoteData(..), WebData)


type alias Widget =
    { color : Color
    , title : String
    , description : String
    , icon : String
    , value : WebData String
    , subvalue : Maybe String
    }


content : Element msg -> Element msg
content e =
    el [ width fill, height fill, paddingXY 30 0 ] e


viewTile : Palette Color -> Element msg -> Element msg
viewTile palette tileContent =
    el
        [ height (px 90)
        , width (fillPortion 1)
        , padding 10
        , Background.color palette.bg2
        , Border.rounded 6
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 15
            , color = rgba 0 0 0 0.1
            }
        , Border.color (Palette.lightish palette.bg2)
        , Border.width 1
        ]
        tileContent


viewWidget : Context a -> Widget -> Element msg
viewWidget ctx widget =
    viewTile ctx.palette
        (row [ spacing 10, centerY ]
            [ el
                [ Background.color ctx.palette.bg1
                , Border.rounded 100
                , height (px 50)
                , width (px 50)
                ]
                (image [ height (px 25), centerY, centerX ]
                    { src = widget.icon, description = "Decorative Icon" }
                )
            , column [ spacing 10 ]
                [ row []
                    [ el [ Font.color (withAlphaEl 0.7 widget.color) ]
                        (text <| String.toUpper widget.title ++ " ")
                    , if widget.description == "" then
                        none

                      else
                        el
                            [ Background.color ctx.palette.bg3
                            , Font.color ctx.palette.fg3
                            , Font.size 10
                            , Border.rounded 10
                            , paddingXY 10 5
                            , stringTooltipAboveWidget ctx (paragraph [ Font.size 14, width (fill |> minimum 300) ] [ text widget.description ])
                            ]
                            (text "i")
                    ]
                , column [ Font.color widget.color, Font.size 25 ]
                    [ remoteDataView ctx.palette text widget.value ]
                , widget.subvalue
                    |> Maybe.map
                        (\subvalue ->
                            el
                                [ Font.color (withAlphaEl 0.5 widget.color) ]
                                (text subvalue)
                        )
                    |> Maybe.withDefault none
                ]
            ]
        )



-- Remote Data Helper


remoteDataView : Palette Color -> (c -> Element msg) -> RemoteData e c -> Element msg
remoteDataView palette successView remoteData =
    case remoteData of
        NotAsked ->
            none

        Loading ->
            loader palette.fg3

        Failure error ->
            badge palette.warning "Error"

        Success data ->
            successView data


loader : Element.Color -> Element msg
loader color =
    let
        defaultConfig =
            Loading.defaultConfig
    in
    el [ padding 4, centerX, centerY ]
        (html <|
            Loading.render
                Loading.DoubleBounce
                { defaultConfig | color = toHex color, size = 22 }
                Loading.On
        )


badge : Color -> String -> Element msg
badge color label =
    el
        [ padding 7
        , Background.color color
        , Border.rounded 1000
        , Font.size 14
        , Font.color <| veryDark color
        , Font.bold
        ]
        (el [ centerX, centerY, moveUp 1 ] (text label))


arrowRight : Element msg
arrowRight =
    el [ paddingXY 8 0 ] (html <| Icons.arrow_right 18)
