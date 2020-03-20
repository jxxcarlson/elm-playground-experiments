module T1 exposing (main, update, view)

{-| Rocket-shaped turtle

    Up arrow: move forward

    Down arrow: move backwards

    Right arrow: rotate counter-clockwise

    Left arrow: rotate clockwise

    Space bar: set angle to zero

    <Enter/Return>: put rocket at origin and set angle to zero

-}

import Playground exposing (..)


main =
    game view
        update
        { x = 0
        , y = 0
        , angle = 0
        }


view computer turtle =
    [ rectangle black computer.screen.width computer.screen.height
    , rocket
        |> move turtle.x turtle.y
        |> rotate turtle.angle
    , turtleInfo computer turtle
    ]


rocket =
    group
        [ circle lightRed 10 |> moveLeft 33
        , rectangle lightYellow 70 25
        , circle blue 4 |> moveRight 25 |> moveUp 5
        , circle blue 4 |> moveRight 25 |> moveDown 5
        , triangle orange 15 |> moveRight 42 |> rotate -90
        ]


turtleInfo computer turtle =
    words white (position turtle) |> moveDown (computer.screen.height / 2 - 20)


position turtle =
    "("
        ++ String.fromInt (round turtle.x)
        ++ ", "
        ++ String.fromInt (round turtle.y)
        ++ ", "
        ++ String.fromInt (round turtle.angle)
        ++ ")"


update computer turtle =
    if computer.keyboard.space then
        { x = turtle.x
        , y = turtle.y
        , angle = 0
        }

    else if computer.keyboard.enter then
        { x = 0
        , y = 0
        , angle = 0
        }

    else
        { x = turtle.x + toY computer.keyboard * cos (degrees turtle.angle)
        , y = turtle.y + toY computer.keyboard * sin (degrees turtle.angle)
        , angle = turtle.angle - toX computer.keyboard
        }
