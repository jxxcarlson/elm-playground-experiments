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
        , y = 75
        , angle = 90
        , radius = 1.0
        , rocketLoad = 0
        , homePlanetStock = 10
        }


view computer turtle =
    [ rectangle black computer.screen.width computer.screen.height
    , bluePlanet computer
    , greenStar computer
    , rocket
        |> move turtle.x turtle.y
        |> rotate turtle.angle
    , infoRockePosition computer turtle
    , infoRocketAngle computer turtle
    , infoBluePlanet computer turtle
    , homePlanet
    ]


homePlanet =
    circle orange
        40



--- ROCKET


distanceToBluePlanet computer turtle =
    let
        dx =
            turtle.x + bpx computer

        dy =
            turtle.y - bpy computer
    in
    sqrt (dx * dx + dy * dy)


rocket =
    group
        [ circle lightRed 10 |> moveLeft 33
        , rectangle lightYellow 70 25
        , circle black 8
        , triangle orange 15 |> moveRight 42 |> rotate -90
        ]


greenStar computer =
    circle green 110
        |> moveRight (computer.screen.width / 2 - 30)
        |> moveDown (0.24 * computer.screen.height)



-- BLUE PLANET


bpx computer =
    computer.screen.width / 2 - 90


bpy computer =
    computer.screen.height / 2 - 90


bpRadius =
    170


bpDockingDistance =
    168


bluePlanet computer =
    circle lightBlue bpRadius
        |> moveLeft (bpx computer)
        |> moveUp (bpy computer)


infoRockePosition computer turtle =
    words white (position turtle)
        |> moveRight (computer.screen.width / 2 - 120)
        |> moveUp (computer.screen.height / 2 - 40)


infoRocketAngle computer turtle =
    words white (angleDisplay turtle)
        |> moveRight (computer.screen.width / 2 - 120)
        |> moveUp (computer.screen.height / 2 - 60)


infoBluePlanet computer turtle =
    words white (bpDistance computer turtle)
        |> moveRight (computer.screen.width / 2 - 120)
        |> moveUp (computer.screen.height / 2 - 90)


position turtle =
    "x, y: "
        ++ String.fromInt (round turtle.x)
        ++ ", "
        ++ String.fromInt (round turtle.y)


angleDisplay turtle =
    "angle: " ++ String.fromInt (modBy 360 (round turtle.angle))


bpDistance computer turtle =
    "bluePlanet: "
        ++ String.fromInt (round (distanceToBluePlanet computer turtle))



--++ "    radius = "
--++ String.fromFloat turtle.radius
--++ ", rocket load = "
--++ String.fromInt (round turtle.rocketLoad)
--++ ", homePlanetStock = "
--++ String.fromInt (round turtle.homePlanetStock)


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
