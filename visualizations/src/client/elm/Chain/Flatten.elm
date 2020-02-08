module Chain.Flatten exposing (..)

import Chain.Build exposing (Block, BlockStatus)
import Circle2d exposing (Circle2d)
import Color exposing (Color, rgb)
import Colors exposing (fromUI, toUI)
import GeometryUtils exposing (TopLeftCoordinates)
import Grid exposing (GridSpec)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias DrawableBlock =
    { hash : String
    , color : Color
    , rect : Rectangle2d Pixels TopLeftCoordinates
    }


type alias DrawableConnector =
    { start : Point2d Pixels TopLeftCoordinates
    , end : Point2d Pixels TopLeftCoordinates
    , color : Color
    }


type alias DrawableNode =
    { circle : Circle2d Pixels TopLeftCoordinates }


type alias DrawableChain =
    { blocks : List DrawableBlock
    , connectors : List DrawableConnector
    , nodes : List DrawableConnector
    , width : Int
    , height : Int
    , numCollapsedBlocksX : Int
    , numCollapsedBlocksY : Int
    }


drawableBlock : GridSpec -> Int -> Int -> Block -> DrawableBlock
drawableBlock gridSpec x y block =
    { hash = block.hash
    , color = blockColor block.status
    , rect = Grid.cell gridSpec x y
    }


drawableConnectors : GridSpec -> Int -> Int -> Block -> List DrawableConnector
drawableConnectors gridSpec x y block =
    List.map
        (\connectToY ->
            let
                block1Rect =
                    Grid.cell gridSpec x y

                block2Rect =
                    Grid.cell gridSpec (x + 1) (y + connectToY)
            in
            { start = Rectangle2d.interpolate block1Rect 1 0.5
            , end = Rectangle2d.interpolate block2Rect 0 0.5
            , color = blockColor block.status
            }
        )
        block.connectors


drawableConnector : GridSpec -> Int -> Int -> Int -> Int -> Color -> DrawableConnector
drawableConnector gridSpec x1 y1 x2 y2 color =
    let
        block1Rect =
            Grid.cell gridSpec x1 y1

        block2Rect =
            Grid.cell gridSpec x2 y2
    in
    { start = Rectangle2d.interpolate block1Rect 1 0.5
    , end = Rectangle2d.interpolate block2Rect 0 0.5
    , color = color
    }


blockColor : BlockStatus -> Color
blockColor status =
    case status of
        Chain.Build.Finalized ->
            Colors.green

        Chain.Build.LastFinalized ->
            Colors.green

        Chain.Build.Candidate ->
            Colors.blue


emptyDrawableChain : DrawableChain
emptyDrawableChain =
    { blocks = []
    , connectors = []
    , nodes = []
    , width = 0
    , height = 0
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


flattenTree : GridSpec -> Int -> Int -> Tree Block -> DrawableChain
flattenTree gridSpec lastFinalizedBlockHeight maxNumVertical chain =
    flattenDepthFirst (Zipper.fromTree chain) gridSpec Nothing ( 0, 0 )
        |> (\{ blocks, connectors, nodes, width, height } ->
                let
                    collapsedH =
                        (Tree.label chain |> .blockHeight)
                            - lastFinalizedBlockHeight

                    collapsedV =
                        max 0 (maxNumVertical - height)
                in
                { blocks = blocks
                , connectors = connectors
                , nodes = nodes
                , width = width
                , height = height
                , numCollapsedBlocksX = collapsedH
                , numCollapsedBlocksY = collapsedV
                }
           )


flattenDepthFirst :
    Zipper Block
    -> GridSpec
    -> Maybe ( Int, Int )
    -> ( Int, Int )
    -> DrawableChain
flattenDepthFirst zipper gridSpec parent ( x, y ) =
    let
        block =
            Zipper.label zipper

        current =
            Just ( x, y )
    in
    (case ( Zipper.firstChild zipper, Zipper.nextSibling zipper ) of
        ( Just child, Just sibling ) ->
            mergeDrawables
                (flattenDepthFirst child gridSpec current ( x + 1, y ))
                (flattenDepthFirst sibling gridSpec current ( x, y + block.forkWidth ))

        ( Just child, Nothing ) ->
            flattenDepthFirst child gridSpec current ( x + 1, y )

        ( Nothing, Just sibling ) ->
            flattenDepthFirst sibling gridSpec current ( x, y + block.forkWidth )

        ( Nothing, Nothing ) ->
            emptyDrawableChain
    )
        |> addDrawables gridSpec parent ( x, y ) (Zipper.label zipper)


mergeDrawables : DrawableChain -> DrawableChain -> DrawableChain
mergeDrawables chainA chainB =
    { blocks = chainA.blocks ++ chainB.blocks
    , connectors = chainA.connectors ++ chainB.connectors
    , nodes = chainA.nodes ++ chainB.nodes
    , width = max chainA.width chainB.width
    , height = max chainA.height chainB.height
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


addDrawables :
    GridSpec
    -> Maybe ( Int, Int )
    -> ( Int, Int )
    -> Block
    -> DrawableChain
    -> DrawableChain
addDrawables gridSpec maybeParent ( x, y ) block chain =
    { blocks = drawableBlock gridSpec x y block :: chain.blocks
    , connectors =
        maybeParent
            |> Maybe.map
                (\( px, py ) ->
                    drawableConnector
                        gridSpec
                        px
                        py
                        x
                        y
                        (blockColor block.status)
                        :: chain.connectors
                )
            |> Maybe.withDefault chain.connectors
    , nodes = []
    , width = max (x + 1) chain.width
    , height = max (y + 1) chain.height
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }
