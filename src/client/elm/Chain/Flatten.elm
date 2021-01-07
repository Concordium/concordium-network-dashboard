module Chain.Flatten exposing (..)

import Chain.Build exposing (Block, BlockStatus(..))
import Chain.Grid as Grid exposing (GridSpec)
import Color exposing (Color)
import Context exposing (Context)
import Element
import Icons exposing (lastBlock)
import Palette exposing (Palette)
import Pixels exposing (Pixels(..))
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Tree exposing (Tree(..), children)
import Tree.Zipper as Zipper exposing (Zipper)


type Coordinates
    = Coordinates


type alias DrawableBlock =
    { hash : String
    , color : Color
    , rect : Rectangle2d Pixels Coordinates
    , fractionNodesAt : Float -- The fraction of nodes for which this is the best block
    }


type alias DrawableConnector =
    { id : String
    , start : Point2d Pixels Coordinates
    , end : Point2d Pixels Coordinates
    , color : Color
    }


type alias DrawableChain =
    { blocks : List DrawableBlock
    , connectors : List DrawableConnector
    , width : Int
    , height : Int
    , viewBoxOffsetX : Float
    , numCollapsedBlocksX : Int
    , numCollapsedBlocksY : Int
    }


type alias DrawableBlockSummaryX =
    { lastFinalized : String
    , numCollapsedBlocks : Int
    , color : Color
    }


drawableBlock : Context a -> GridSpec -> Block -> DrawableBlock
drawableBlock ctx gridSpec block =
    { hash = block.hash
    , color = blockColor ctx.palette block.status
    , rect = Grid.cell gridSpec block.x block.y
    , fractionNodesAt = block.fractionNodesAt
    }


drawableConnector : Context a -> GridSpec -> Block -> Block -> DrawableConnector
drawableConnector ctx gridSpec blockA blockB =
    let
        block1Rect =
            Grid.cell gridSpec blockA.x blockA.y

        block2Rect =
            Grid.cell gridSpec blockB.x blockB.y
    in
    { id = String.left 4 blockA.hash ++ String.left 4 blockB.hash
    , start = Rectangle2d.interpolate block1Rect 1 0.5
    , end = Rectangle2d.interpolate block2Rect 0 0.5
    , color = blockColor ctx.palette blockB.status
    }


mergeDrawables : DrawableChain -> DrawableChain -> DrawableChain
mergeDrawables chainA chainB =
    { blocks = chainA.blocks ++ chainB.blocks
    , connectors = chainA.connectors ++ chainB.connectors
    , width = max chainA.width chainB.width
    , height = max chainA.height chainB.height
    , viewBoxOffsetX = chainB.viewBoxOffsetX
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


blockColor : Palette Element.Color -> BlockStatus -> Color
blockColor palette status =
    Palette.uiToColor <|
        case status of
            Finalized ->
                palette.c2

            LastFinalized ->
                palette.c2

            Candidate ->
                palette.c1

            Discarded ->
                palette.deactivated


emptyDrawableChain : DrawableChain
emptyDrawableChain =
    { blocks = []
    , connectors = []
    , width = 0
    , height = 0
    , viewBoxOffsetX = 0
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


{-| Convert tree into a DrawableChain which contains all geometric components (including coordinates, collapsed blocks, and offsets) to be rendered.
-}
flattenTree : Context a -> GridSpec -> Int -> Int -> Tree Block -> DrawableChain
flattenTree ctx gridSpec lastFinalizedBlockHeight maxNumVertical chain =
    let
        flattenedBlocks =
            chain |> Tree.flatten |> List.map (drawableBlock ctx gridSpec)

        firstBlockHeight =
            Tree.label chain |> .x

        lastBlockHeight =
            (chain |> Tree.flatten |> List.map .x |> List.maximum |> Maybe.withDefault 1) + 1

        width =
            lastBlockHeight - firstBlockHeight

        height =
            (chain |> Tree.flatten |> List.map .y |> List.maximum |> Maybe.withDefault 1) + 1

        collapsedX =
            firstBlockHeight - lastFinalizedBlockHeight

        collapsedY =
            max 0 (maxNumVertical - height)

        offsetX =
            Grid.intersection gridSpec lastBlockHeight 0
                |> Point2d.toPixels
                |> .x
    in
    { blocks = flattenedBlocks
    , connectors = flattenConnectors ctx gridSpec chain
    , width = width
    , height = height
    , viewBoxOffsetX = offsetX
    , numCollapsedBlocksX = collapsedX
    , numCollapsedBlocksY = collapsedY
    }


flattenConnectors : Context a -> GridSpec -> Tree Block -> List DrawableConnector
flattenConnectors ctx gridSpec tree =
    case Tree.children tree of
        [] ->
            []

        children ->
            List.map
                (\child ->
                    drawableConnector ctx gridSpec (Tree.label tree) (Tree.label child)
                        :: flattenConnectors ctx gridSpec child
                )
                children
                |> List.concat
