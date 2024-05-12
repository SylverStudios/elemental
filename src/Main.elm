module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Dict.Any exposing (AnyDict)
import Html exposing (Html, br, button, div, h2, p, span, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import List.Extra



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Model
    = Model { selected : List Element }


type Element
    = Fire
    | Water
    | Earth


type Spell
    = Fireball
    | Ember
    | WaterJet
    | EarthSpike
    | Steam
    | Lava
    | Mud
    | Fizzle


displaySpellbook : List (Html Msg)
displaySpellbook =
    spellbook
        |> Dict.Any.toList
        |> List.map (\( elementList, spell ) -> displayGrouping (spellToSting spell) elementList)


spellToSting : Spell -> String
spellToSting spell =
    case spell of
        Ember ->
            "Ember"

        Fireball ->
            "Fireball"

        WaterJet ->
            "WaterJet"

        EarthSpike ->
            "EarthSpike"

        Steam ->
            "Steam"

        Lava ->
            "Lava"

        Mud ->
            "Mud"

        Fizzle ->
            "Fizzle"


spellbook : AnyDict (List Int) (List Element) Spell
spellbook =
    let
        elementToInt element =
            case element of
                Fire ->
                    0

                Water ->
                    1

                Earth ->
                    2

        elementsToInt listOfElements =
            List.map elementToInt listOfElements
    in
    Dict.Any.empty elementsToInt
        |> Dict.Any.insert [ Fire ] Ember
        |> Dict.Any.insert [ Fire, Fire ] Fireball
        |> Dict.Any.insert [ Fire, Water ] Steam
        |> Dict.Any.insert [ Fire, Earth ] Lava
        |> Dict.Any.insert [ Water, Fire ] Steam
        |> Dict.Any.insert [ Water, Water ] WaterJet
        |> Dict.Any.insert [ Water, Earth ] Mud
        |> Dict.Any.insert [ Earth, Fire ] Lava
        |> Dict.Any.insert [ Earth, Water ] Mud
        |> Dict.Any.insert [ Earth, Earth ] EarthSpike


init : Model
init =
    Model { selected = [] }



-- UPDATE


type Msg
    = Add Element
    | Backspace
    | Remove Int


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        Add element ->
            Model { model | selected = element :: model.selected }

        Backspace ->
            Model { model | selected = model.selected |> List.tail |> Maybe.withDefault [] }

        Remove index ->
            Model { model | selected = List.Extra.removeAt index model.selected }



-- VIEW


view : Model -> Html Msg
view (Model model) =
    div [ style "display" "flex", style "align-content" "flex-start" ]
        [ div [ id "spellbook" ]
            [ h2 [] [ text "Spellbook" ]
            , div [ style "display" "flex", style "flex-direction" "column" ] displaySpellbook
            ]
        , div [ id "selection", style "flex-grow" "2", style "margin-left" "10%" ]
            [ h2 [] [ text "Add" ]
            , div [ style "display" "flex" ]
                [ dot (Add Earth) Earth
                , dot (Add Fire) Fire
                , dot (Add Water) Water
                ]
            , br [] []
            , text "Selected"
            , div [] [ button [ onClick Backspace ] [ text "Backspace" ] ]
            , div [] (List.indexedMap (\index e -> dot (Remove index) e) model.selected)
            , div [ id "results" ]
                [ h2 [] [ text "Results" ]
                , div [ style "display" "flex" ] (parseAsGrouping model.selected)
                , br [] []
                ]
            ]
        ]


parseSpell : List Element -> Html Msg
parseSpell elements =
    p [] <|
        List.foldl
            (\a b ->
                text (elementToString a) :: b
            )
            []
            elements


displayGrouping : String -> List Element -> Html Msg
displayGrouping name elements =
    div [ style "background-color" "#CBC3E3", style "margin" "5px", style "border-radius" "5px", style "padding" "5px" ]
        [ span [ style "text-align" "center" ] [ text name ]
        , br [] []
        , span [] <| List.indexedMap (\index e -> dot (Remove index) e) elements
        ]


parseAsGrouping : List Element -> List (Html Msg)
parseAsGrouping elements =
    case elements of
        a :: b :: rest ->
            case castSpell [ a, b ] of
                Just spell ->
                    displayGrouping (spellToSting spell) [ a, b ] :: parseAsGrouping rest

                Nothing ->
                    let
                        singleCast =
                            castSpell [ a ]
                                |> Maybe.map (\spell -> displayGrouping (spellToSting spell) [ a ])
                                |> Maybe.withDefault (displayGrouping (spellToSting Fizzle) [ a ])
                    in
                    singleCast :: parseAsGrouping (b :: rest)

        [ e ] ->
            case castSpell [ e ] of
                Just spell ->
                    [ displayGrouping (spellToSting spell) [ e ] ]

                Nothing ->
                    [ displayGrouping (spellToSting Fizzle) [ e ] ]

        [] ->
            [ text "" ]


castSpell : List Element -> Maybe Spell
castSpell elements =
    Dict.Any.get elements spellbook


doubleParseSpell elements =
    case elements of
        a :: b :: rest ->
            case doubleSpell a b of
                Just spellName ->
                    spellName ++ doubleParseSpell rest

                Nothing ->
                    elementToString a ++ doubleParseSpell (b :: rest)

        [ e ] ->
            elementToString e

        [] ->
            ""


elementToString : Element -> String
elementToString element =
    case element of
        Fire ->
            "Fire"

        Water ->
            "Water"

        Earth ->
            "Earth"


doubleSpell : Element -> Element -> Maybe String
doubleSpell element1 element2 =
    case ( element1, element2 ) of
        ( Fire, Water ) ->
            Just "Steam"

        ( Water, Fire ) ->
            Just "Steam"

        ( Fire, Earth ) ->
            Just "Lava"

        ( Earth, Fire ) ->
            Just "Lava"

        ( Water, Earth ) ->
            Just "Mud"

        ( Earth, Water ) ->
            Just "Mud"

        ( Fire, Fire ) ->
            Nothing

        ( Water, Water ) ->
            Nothing

        ( Earth, Earth ) ->
            Nothing


dot : Msg -> Element -> Html Msg
dot msg element =
    let
        color : String
        color =
            case element of
                Fire ->
                    "red"

                Water ->
                    "blue"

                Earth ->
                    "brown"
    in
    button
        [ onClick msg
        , style "height" "25px"
        , style "width" "25px"
        , style "background-color" color
        , style "border-radius" "50%"
        , style "display" "inline-block"
        ]
        []


simpleDot : Element -> Html Msg
simpleDot element =
    let
        color : String
        color =
            case element of
                Fire ->
                    "red"

                Water ->
                    "blue"

                Earth ->
                    "brown"
    in
    button
        [ style "height" "25px"
        , style "width" "25px"
        , style "background-color" color
        , style "border-radius" "50%"
        , style "display" "inline-block"
        ]
        []
