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
import List exposing (length)
import List.Extra



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Model
    = Model
        { selected : List Element
        , hand : List SelectableElement
        , view : View
        , lastCast : Maybe (List Element)
        , castHistory : List Resolution
        }


type Resolution
    = Resolution
        { elements : List Element
        , results : List Instance
        }


type Instance
    = Instance
        { elements : List Element
        , spell : Spell
        , damage : Int
        , effects : List String
        }


type View
    = Discover
    | Play


type SelectableElement
    = SelectableElement Element Bool


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


init : Model
init =
    Model
        { selected = []
        , hand = []
        , view = Discover
        , lastCast = Nothing
        , castHistory = []
        }



-- UPDATE


type Msg
    = Add Element
    | Backspace
    | Remove Int
    | Draw
    | ToggleElement Int
    | ToggleView
    | Cast


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        Add element ->
            Model { model | selected = element :: model.selected }

        Backspace ->
            Model { model | selected = model.selected |> List.tail |> Maybe.withDefault [] }

        Remove index ->
            Model { model | selected = List.Extra.removeAt index model.selected }

        Draw ->
            Model { model | hand = List.append model.hand [ SelectableElement Fire False, SelectableElement Water False, SelectableElement Earth False ] }

        ToggleElement index ->
            let
                newHand =
                    List.Extra.updateAt index (\(SelectableElement element selected) -> SelectableElement element (not selected)) model.hand
            in
            Model { model | hand = newHand }

        ToggleView ->
            case model.view of
                Discover ->
                    Model { model | view = Play }

                Play ->
                    Model { model | view = Discover }

        Cast ->
            let
                ( selectedElements, unselectedElements ) =
                    List.partition (\(SelectableElement _ s) -> s) model.hand

                resolution =
                    resolveElements (List.map (\(SelectableElement element _) -> element) selectedElements)
            in
            Model
                { model
                    | hand = unselectedElements
                    , lastCast = Just (List.map (\(SelectableElement element _) -> element) selectedElements)
                    , castHistory = resolution :: model.castHistory
                }


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



-- VIEW


view : Model -> Html Msg
view (Model model) =
    case model.view of
        Discover ->
            div [ style "background-color" "#f28d85", style "border-radius" "5px" ] [ button [ onClick ToggleView ] [ text "Toggle" ], viewDiscover model.selected ]

        Play ->
            div [ style "background-color" "#bff2aa", style "border-radius" "5px" ] [ button [ onClick ToggleView ] [ text "Toggle" ], viewPlay model.hand model.castHistory ]


viewPlay : List SelectableElement -> List Resolution -> Html Msg
viewPlay hand castHistory =
    div []
        [ div [ style "display" "flex", style "justify-content" "space-between" ]
            [ div []
                [ text <| String.fromInt <| length <| hand
                , button [ onClick Draw ] [ text "Draw" ]
                ]
            , div [ id "hand", style "display" "flex" ]
                [ h2 [] [ text "Hand" ]
                , div [ style "display" "flex", style "flex-direction" "row" ] (List.indexedMap (\index se -> viewSelectableElement (ToggleElement index) se) hand)
                ]
            , div [ id "on-deck", style "display" "flex" ]
                [ h2 [] [ text "On Deck" ]
                , div [ style "display" "flex", style "flex-direction" "row" ]
                    (parseAsGrouping (hand |> List.filter (\(SelectableElement _ s) -> s) |> List.map (\(SelectableElement element _) -> element)))
                ]
            ]
        , div [ style "display" "flex" ]
            [ h2 [] [ text "Resolution" ]
            , text "tmp"
            , button [ onClick Cast ] [ text "Cast" ]
            ]
        , div [ id "history" ]
            [ h2 [] [ text "History" ]
            , div [ style "display" "flex" ]
                [ div []
                    [ text "Turns: "
                    , text <| String.fromInt <| length castHistory
                    , br [] []
                    , text "Damage: "
                    , text <| String.fromInt <| List.foldl (\(Resolution data) acc -> List.foldl (\(Instance instance) iAcc -> iAcc + instance.damage) acc data.results) 0 castHistory
                    ]
                , div [] (List.map viewResolution castHistory)
                ]
            ]
        ]


viewDiscover : List Element -> Html Msg
viewDiscover selected =
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
            , div [] (List.indexedMap (\index e -> dot (Remove index) e) selected)
            , div [ id "results" ]
                [ h2 [] [ text "Results" ]
                , div [ style "display" "flex" ] (parseAsGrouping selected)
                , br [] []
                ]
            ]
        ]


displayGrouping : String -> List Element -> Html Msg
displayGrouping name elements =
    div [ style "background-color" "#CBC3E3", style "margin" "5px", style "border-radius" "5px", style "padding" "5px" ]
        [ span [ style "text-align" "center" ] [ text name ]
        , br [] []
        , span [] <| List.indexedMap (\index e -> dot (Remove index) e) elements
        ]


resolveElements : List Element -> Resolution
resolveElements elements =
    Resolution { elements = elements, results = foldElements elements }


{-| I can only handle 2 element combos currently
-}
foldElements : List Element -> List Instance
foldElements elements =
    case elements of
        a :: b :: rest ->
            case castSpell [ a, b ] of
                Just spell ->
                    Instance
                        { elements = [ a, b ]
                        , spell = spell
                        , damage = damage spell
                        , effects = effects spell
                        }
                        :: foldElements rest

                Nothing ->
                    let
                        spell : Spell
                        spell =
                            castSpell [ a ] |> Maybe.withDefault Fizzle
                    in
                    Instance
                        { elements = [ a ]
                        , spell = spell
                        , damage = damage spell
                        , effects = effects spell
                        }
                        :: foldElements (b :: rest)

        [ e ] ->
            case castSpell [ e ] of
                Just spell ->
                    [ Instance
                        { elements = [ e ]
                        , spell = spell
                        , damage = damage spell
                        , effects = effects spell
                        }
                    ]

                Nothing ->
                    [ Instance
                        { elements = [ e ]
                        , spell = Fizzle
                        , damage = damage Fizzle
                        , effects = effects Fizzle
                        }
                    ]

        [] ->
            []


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


viewResolution : Resolution -> Html Msg
viewResolution (Resolution data) =
    let
        viewInstance : Instance -> Html Msg
        viewInstance (Instance instance) =
            div [ style "display" "flex" ]
                [ displayGrouping (spellToSting instance.spell) instance.elements
                , div [] [ text "Damage: ", text (String.fromInt instance.damage) ]
                , div [] (text " Effects: " :: List.map (\e -> text e) instance.effects)
                ]
    in
    div [] (List.map viewInstance data.results)


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


damage : Spell -> Int
damage spell =
    case spell of
        Ember ->
            1

        Fireball ->
            3

        WaterJet ->
            2

        EarthSpike ->
            2

        Steam ->
            2

        Lava ->
            4

        Mud ->
            3

        Fizzle ->
            0


effects : Spell -> List String
effects spell =
    case spell of
        Ember ->
            [ "Burn" ]

        Fireball ->
            [ "Burn" ]

        WaterJet ->
            [ "Wet" ]

        EarthSpike ->
            [ "Pierce" ]

        Steam ->
            [ "Wet", "Burn" ]

        Lava ->
            [ "Burn", "Pierce" ]

        Mud ->
            [ "Wet", "Pierce" ]

        Fizzle ->
            []


dot : Msg -> Element -> Html Msg
dot msg element =
    button
        [ onClick msg
        , style "height" "25px"
        , style "width" "25px"
        , style "background-color" (elementColor element)
        , style "border-radius" "50%"
        , style "display" "inline-block"
        ]
        []


elementColor : Element -> String
elementColor element =
    case element of
        Fire ->
            "red"

        Water ->
            "blue"

        Earth ->
            "brown"


simpleDot : Element -> Html Msg
simpleDot element =
    button
        [ style "height" "25px"
        , style "width" "25px"
        , style "background-color" (elementColor element)
        , style "border-radius" "50%"
        , style "display" "inline-block"
        ]
        []


viewSelectableElement : Msg -> SelectableElement -> Html Msg
viewSelectableElement msg (SelectableElement element selected) =
    let
        selectedStyle =
            if selected then
                style "align-self" "flex-start"

            else
                style "align-self" "center"
    in
    div [ style "display" "flex" ]
        [ button
            [ onClick msg
            , style "height" "25px"
            , style "width" "25px"
            , style "background-color" (elementColor element)
            , style "border-radius" "50%"
            , style "display" "inline-block"
            , selectedStyle
            ]
            []
        ]
