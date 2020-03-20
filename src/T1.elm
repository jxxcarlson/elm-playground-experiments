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
import Set


main =
    game view
        update
        { x = 0
        , y = 0
        , angle = 0
        , radius = 1.0
        }


view computer turtle =
    [ rectangle black computer.screen.width computer.screen.height
    , blueStar computer
    , greenStar computer
    , rocket
        |> move turtle.x turtle.y
        |> rotate turtle.angle
    , info computer turtle
    , origin
    ]


origin =
    circle orange
        10


rocket =
    group
        [ circle lightRed 10 |> moveLeft 33
        , rectangle lightYellow 70 25
        , circle blue 4 |> moveRight 25 |> moveUp 5
        , circle blue 4 |> moveRight 25 |> moveDown 5
        , triangle orange 15 |> moveRight 42 |> rotate -90
        ]


greenStar computer =
    circle green 110
        |> moveRight (computer.screen.width / 2 - 30)
        |> moveDown (0.24 * computer.screen.height)


blueStar computer =
    circle lightBlue 170
        |> moveLeft (computer.screen.width / 2 - 90)
        |> moveUp (computer.screen.height / 2 - 90)


info computer turtle =
    words white (position turtle) |> moveDown (computer.screen.height / 2 - 20)


position turtle =
    "x, y = "
        ++ String.fromInt (round turtle.x)
        ++ ", "
        ++ String.fromInt (round turtle.y)
        ++ "    angle = "
        ++ String.fromInt (modBy 360 (round turtle.angle))
        ++ "    radius = "
        ++ String.fromFloat turtle.radius


update computer turtle =
    if computer.keyboard.space then
        { turtle | angle = 0 }

    else if computer.keyboard.enter then
        { turtle | x = 0, y = 0, angle = 0, radius = 1 }

    else if Set.member "=" computer.keyboard.keys then
        { turtle | radius = turtle.radius + 0.1 }

    else if Set.member "-" computer.keyboard.keys then
        { turtle | radius = turtle.radius - 0.1 }

    else if Set.member "0" computer.keyboard.keys then
        { turtle | radius = 1 }

    else
        { turtle
            | x = turtle.x + toY computer.keyboard * turtle.radius * cos (degrees turtle.angle)
            , y = turtle.y + toY computer.keyboard * turtle.radius * sin (degrees turtle.angle)
            , angle = turtle.angle - toX computer.keyboard
        }
