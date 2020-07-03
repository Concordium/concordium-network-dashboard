module Chain.Flatten exposing (..)

import Chain.Build exposing (Block, BlockStatus(..))
import Circle2d exposing (Circle2d)
import Color exposing (Color, rgb)
import Context exposing (Context)
import Element
import GeometryUtils exposing (TopLeftCoordinates)
import Grid exposing (GridSpec)
import List.Extra as List
import Palette exposing (Palette)
import Pixels exposing (Pixels(..))
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Vector2d exposing (Vector2d)


type alias DrawableBlock =
    { hash : String
    , color : Color
    , rect : Rectangle2d Pixels TopLeftCoordinates
    }


type alias DrawableConnector =
    { id : String
    , start : Point2d Pixels TopLeftCoordinates
    , end : Point2d Pixels TopLeftCoordinates
    , color : Color
    }


type alias DrawableNode =
    { nodeId : String
    , circle : Circle2d Pixels TopLeftCoordinates
    }


type alias DrawableChain =
    { blocks : List DrawableBlock
    , connectors : List DrawableConnector
    , nodes : List DrawableNode
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


drawableBlock : Context a -> GridSpec -> Int -> Block -> DrawableBlock
drawableBlock ctx gridSpec y block =
    { hash = block.hash
    , color = blockColor ctx.palette block.status
    , rect = Grid.cell gridSpec block.blockHeight y
    }


drawableConnector :
    GridSpec
    -> Block
    -> Block
    -> Int
    -> Int
    -> Color
    -> DrawableConnector
drawableConnector gridSpec blockA blockB yA yB color =
    let
        block1Rect =
            Grid.cell gridSpec blockA.blockHeight yA

        block2Rect =
            Grid.cell gridSpec blockB.blockHeight yB
    in
    { id = String.left 4 blockA.hash ++ String.left 4 blockB.hash
    , start = Rectangle2d.interpolate block1Rect 1 0.5
    , end = Rectangle2d.interpolate block2Rect 0 0.5
    , color = color
    }


drawableNodes : GridSpec -> Int -> Block -> List DrawableNode
drawableNodes gridSpec y block =
    let
        ( size, nx ) =
            ( 4, 8 )

        blockRect =
            Grid.cell gridSpec block.blockHeight y

        nodeFromIndices : String -> Int -> Int -> DrawableNode
        nodeFromIndices nodeId xp yp =
            Rectangle2d.interpolate blockRect (toFloat (xp + 1) / toFloat (nx + 1)) -0.15
                |> Point2d.translateBy (Vector2d.pixels 0 (toFloat size * toFloat yp * -1.5))
                |> Circle2d.withRadius (Pixels.pixels (toFloat size / 2.0))
                |> (\circle -> { nodeId = nodeId, circle = circle })

        row iy elements =
            elements
                |> List.indexedMap
                    (\ix nodeId ->
                        nodeFromIndices nodeId ix iy
                    )

        onTop =
            Rectangle2d.interpolate blockRect 0.5 -0.2
    in
    block.nodesAt
        |> List.greedyGroupsOf nx
        |> List.indexedMap row
        |> List.concat


simplerDrawableNodes : GridSpec -> Int -> Block -> List DrawableNode
simplerDrawableNodes gridSpec y block =
    let
        ( size, nx ) =
            ( 2 + 4 * (toFloat block.numNodesAt / 5), 8 )

        blockRect =
            Grid.cell gridSpec block.blockHeight y
    in
    block.nodesAt
        |> List.map
            (\nodeId ->
                Rectangle2d.interpolate blockRect 0.5 (-0.2 * (1 + (toFloat block.numNodesAt / 20)))
                    |> Circle2d.withRadius (Pixels.pixels (size / 2.0))
                    |> (\circle -> { nodeId = nodeId, circle = circle })
            )


mergeDrawables : DrawableChain -> DrawableChain -> DrawableChain
mergeDrawables chainA chainB =
    { blocks = chainA.blocks ++ chainB.blocks
    , connectors = chainA.connectors ++ chainB.connectors
    , nodes = chainA.nodes ++ chainB.nodes
    , width = max chainA.width chainB.width
    , height = max chainA.height chainB.height
    , viewBoxOffsetX = chainB.viewBoxOffsetX
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


addDrawables :
    Context a
    -> GridSpec
    -> Maybe ( Int, Int )
    -> Maybe Block
    -> ( Int, Int )
    -> Block
    -> DrawableChain
    -> DrawableChain
addDrawables ctx gridSpec maybeParent maybeParentBlock ( x, y ) block chain =
    { blocks = drawableBlock ctx gridSpec y block :: chain.blocks
    , connectors =
        case ( maybeParent, maybeParentBlock ) of
            ( Just ( xp, yp ), Just parent ) ->
                drawableConnector
                    gridSpec
                    parent
                    block
                    yp
                    y
                    (blockColor ctx.palette block.status)
                    :: chain.connectors

            _ ->
                chain.connectors
    , nodes = drawableNodes gridSpec y block ++ chain.nodes
    , width = max (x + 1) chain.width
    , height = max (y + 1) chain.height
    , viewBoxOffsetX = chain.viewBoxOffsetX
    , numCollapsedBlocksX = 0
    , numCollapsedBlocksY = 0
    }


blockColor : Palette Element.Color -> BlockStatus -> Color
blockColor palette status =
    case status of
        Finalized ->
            palette.c2 |> Palette.uiToColor

        LastFinalized ->
            palette.c2 |> Palette.uiToColor

        Candidate ->
            palette.c1 |> Palette.uiToColor


emptyDrawableChain : DrawableChain
emptyDrawableChain =
    { blocks = []
    , connectors = []
    , nodes = []
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
        { blocks, connectors, nodes, width, height } =
            flattenDepthFirst ctx (Zipper.fromTree chain) gridSpec Nothing Nothing ( 0, 0 )

        firstBlockHeight =
            Tree.label chain |> .blockHeight

        collapsedX =
            firstBlockHeight - lastFinalizedBlockHeight

        collapsedY =
            max 0 (maxNumVertical - height)

        offsetX =
            Grid.intersection gridSpec firstBlockHeight 0
                |> Point2d.toPixels
                |> .x
    in
    { blocks = blocks
    , connectors = connectors
    , nodes = nodes
    , width = width
    , height = height
    , viewBoxOffsetX = offsetX
    , numCollapsedBlocksX = collapsedX
    , numCollapsedBlocksY = collapsedY
    }


{-| Convert zipper (representing a subtree) into a DrawableChain which contains all geometric components (including coordinates) to be rendered.
-}
flattenDepthFirst :
    Context a
    -> Zipper Block
    -> GridSpec
    -> Maybe ( Int, Int )
    -> Maybe Block
    -> ( Int, Int )
    -> DrawableChain
flattenDepthFirst ctx zipper gridSpec parentCoords parentBlock ( x, y ) =
    let
        block =
            Zipper.label zipper

        current =
            Just ( x, y )
    in
    (case ( Zipper.firstChild zipper, Zipper.nextSibling zipper ) of
        ( Just child, Just sibling ) ->
            mergeDrawables
                (flattenDepthFirst ctx child gridSpec current (Just block) ( x + 1, y ))
                (flattenDepthFirst ctx sibling gridSpec parentCoords parentBlock ( x, y + block.forkWidth ))

        ( Just child, Nothing ) ->
            flattenDepthFirst ctx child gridSpec current (Just block) ( x + 1, y )

        ( Nothing, Just sibling ) ->
            flattenDepthFirst ctx sibling gridSpec parentCoords parentBlock ( x, y + block.forkWidth )

        ( Nothing, Nothing ) ->
            emptyDrawableChain
    )
        |> addDrawables ctx gridSpec parentCoords parentBlock ( x, y ) block
