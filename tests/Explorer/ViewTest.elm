module Explorer.ViewTest exposing (..)

import Element exposing (..)
import Expect exposing (Expectation, equalLists)
import Explorer.View exposing (displayUpdateInfo)
import Test exposing (..)


suite : Test
suite =
    let
        actual =
            List.map (displayUpdateInfo "Name")
                [ ""
                , "..."
                , "      .. .      ..    "
                , "     "
                , "a.  ..    .....  ."
                , "..   ... a.  ..    .....  ."
                , "..\n...   a... \n..  ..aa .."
                , "\n\n\n"
                ]

        expected =
            List.map text
                [ ""
                , ""
                , ""
                , ""
                , "Name: a. "
                , "Name: ..   ... a. "
                , "Name: ..\n...   a... \n..  ..aa. "
                , "Name: \n\n\n. "
                ]
    in
    describe "The Explorer.View module, displayStr function"
        [ test "Displays update-transaction-description parts correctly" <|
            \_ -> equalLists expected actual
        ]
