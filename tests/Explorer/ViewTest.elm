module Explorer.ViewTest exposing (..)

import Expect exposing (Expectation, equalLists)
import Test exposing (..)
import Explorer.View exposing (displayStr)
import Element exposing (..)

suite : Test
suite =
    let actual = List.map (displayStr "Name") [ ""
                                              , "..."
                                              , "      .. .      ..    "
                                              , "     "
                                              , "a.  ..    .....  ."
                                              , "..   ... a.  ..    .....  ."
                                              , "..\n...   a... \n..  ..aa .."
                                              , "\n\n\n"
                                              ]
        expected = List.map text [ ""
                                 , ""
                                 , ""
                                 , ""
                                 , "Name: a. "
                                 , "Name: ..   ... a. "
                                 , "Name: ..\n...   a... \n..  ..aa. "
                                 , "Name: \n\n\n. "
                                 ]
    in describe "The Explorer.View module, displayStr function"
        [ test "Displays update-transaction-description parts correctly" <|
            \_ -> equalLists expected actual
        ]
