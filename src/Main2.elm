module Main2 exposing (..)

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


type Game
    = Selection (List SelectableElement) Int Int (List Resolution)
    | Castable (List SelectableElement) Int Int (List Resolution)
    | Done (List Resolution)


type Resolution
    = Resolution
        { elements : List Element
        , results : List SpellConfiguration
        }


type SpellConfiguration
    = SpellConfiguration
        { elements : List Element
        , spell : Spell
        , life : Int
        , effects : List String
        , name : String
        }


type View
    = Discover
    | Demo
    | Game Game


type SelectableElement
    = SelectableElement Element Bool


type Element
    = Fire
    | Water
    | Earth
    | Wind


type Spell
    = Sprout
    | Growth
    | Burn
    | Wildfire
    | Rain
    | Pour
    | Storm
    | Twister


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
    | Play
    | GameCast
    | GameToggleElement Int


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
                    Model { model | view = Demo }

                Demo ->
                    Model { model | view = Discover }

                Game _ ->
                    Model model

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

        Play ->
            Model { model | view = Game (Selection [ SelectableElement Fire False, SelectableElement Water False, SelectableElement Earth False, SelectableElement Wind False ] 10 10 []) }

        GameCast ->
            let
                newGameState =
                    case model.view of
                        Game game ->
                            case game of
                                Done x ->
                                    Done x

                                Selection hand player enemy castHistory ->
                                    Selection hand player enemy castHistory

                                Castable hand player enemy castHistory ->
                                    let
                                        ( selectedElements, unselectedElements ) =
                                            List.partition (\(SelectableElement _ s) -> s) hand

                                        resolution =
                                            resolveElements (List.map (\(SelectableElement element _) -> element) selectedElements)

                                        damage =
                                            List.foldl (\(Resolution data) acc -> List.foldl (\(SpellConfiguration instance) iAcc -> iAcc + instance.life) acc data.results) 0 castHistory
                                    in
                                    if enemy - damage <= 0 then
                                        Done (resolution :: castHistory)

                                    else
                                        Selection (addDraw unselectedElements) player (enemy - damage) (resolution :: castHistory)

                        _ ->
                            Selection [] 0 0 []
            in
            Model { model | view = Game newGameState }

        GameToggleElement index ->
            let
                newGameState =
                    case model.view of
                        Game game ->
                            case game of
                                Done x ->
                                    Done x

                                Selection hand player enemy castHistory ->
                                    Castable
                                        (List.Extra.updateAt index (\(SelectableElement element selected) -> SelectableElement element (not selected)) hand)
                                        player
                                        enemy
                                        castHistory

                                Castable hand player enemy castHistory ->
                                    let
                                        newHand =
                                            List.Extra.updateAt index (\(SelectableElement element selected) -> SelectableElement element (not selected)) hand
                                    in
                                    if List.any (\(SelectableElement _ s) -> s) newHand then
                                        Castable newHand player enemy castHistory

                                    else
                                        Selection newHand player enemy castHistory

                        _ ->
                            Selection [] 0 0 []
            in
            Model { model | view = Game newGameState }


addDraw : List SelectableElement -> List SelectableElement
addDraw hand =
    List.append hand [ SelectableElement Fire False, SelectableElement Water False, SelectableElement Earth False ]


displaySpellbook : Html Msg
displaySpellbook =
    div
        [ style "height" "300px"
        , style "overflow-y" "auto"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "padding" "10px"
        ]
        [ div [ style "font-weight" "bold", style "margin-bottom" "10px" ] [ text "Spellbook" ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "5px" ]
            (spellbook
                |> Dict.Any.toList
                |> List.map (\( _, spell ) -> displayGrouping (spellConfiguration spell))
            )
        ]


spellToSting : Spell -> String
spellToSting =
    let
        fx (SpellConfiguration { name }) =
            name
    in
    spellConfiguration >> fx


spellLife : Spell -> Int
spellLife =
    let
        fx (SpellConfiguration { life }) =
            life
    in
    spellConfiguration >> fx


spellEffects : Spell -> List String
spellEffects =
    let
        fx (SpellConfiguration { effects }) =
            effects
    in
    spellConfiguration >> fx


spellConfiguration : Spell -> SpellConfiguration
spellConfiguration spell =
    case spell of
        Sprout ->
            SpellConfiguration
                { elements = [ Earth ]
                , spell = Sprout
                , life = 1
                , effects = [ "+1 Earth" ]
                , name = "Sprout"
                }

        Growth ->
            SpellConfiguration
                { elements = [ Earth, Earth ]
                , spell = Growth
                , life = 2
                , effects = [ "+1 Earth", "+1 Regen" ]
                , name = "Growth"
                }

        Burn ->
            SpellConfiguration
                { elements = [ Fire ]
                , spell = Burn
                , life = -1
                , effects = [ "+1 Fire" ]
                , name = "Burn"
                }

        Wildfire ->
            SpellConfiguration
                { elements = [ Fire, Fire ]
                , spell = Wildfire
                , life = -1
                , effects = [ "+1 Burn", "+1 Fire" ]
                , name = "Wildfire"
                }

        Rain ->
            SpellConfiguration
                { elements = [ Water ]
                , spell = Rain
                , life = 0
                , effects = [ "+1 Regen", "Cleanse", "+1 Water" ]
                , name = "Rain"
                }

        Pour ->
            SpellConfiguration
                { elements = [ Water, Water ]
                , spell = Pour
                , life = 0
                , effects = [ "+1 Regen", "Cleanse", "+1 Water", "+1 Storm" ]
                , name = "Pour"
                }

        Storm ->
            SpellConfiguration
                { elements = [ Wind ]
                , spell = Storm
                , life = 0
                , effects = [ "+1 Storm", "+1 Wind" ]
                , name = "Storm"
                }

        Twister ->
            SpellConfiguration
                { elements = [ Wind, Wind ]
                , spell = Twister
                , life = -1
                , effects = [ "+2 Storm", "+1 Wind" ]
                , name = "Twister"
                }


spellbook : AnyDict (List Int) (List Element) Spell
spellbook =
    let
        spells : List Spell
        spells =
            [ Sprout, Growth, Burn, Wildfire, Rain, Pour, Storm, Twister ]

        elementToInt element =
            case element of
                Fire ->
                    0

                Water ->
                    1

                Earth ->
                    2

                Wind ->
                    3

        elementsToInt listOfElements =
            List.map elementToInt listOfElements

        spellConfigToComparable (SpellConfiguration record) =
            { elements = record.elements, spell = record.spell }
    in
    List.foldl (\spell acc -> Dict.Any.insert spell.elements spell.spell acc) (Dict.Any.empty elementsToInt) (spells |> List.map spellConfiguration |> List.map spellConfigToComparable)



-- VIEW


view : Model -> Html Msg
view (Model model) =
    case model.view of
        Discover ->
            div [ style "background-color" "#f28d85", style "border-radius" "5px" ] [ button [ onClick ToggleView ] [ text "Toggle" ], button [ onClick Play ] [ text "Play!" ], viewDiscover model.selected ]

        Demo ->
            div [ style "background-color" "#bff2aa", style "border-radius" "5px" ] [ button [ onClick ToggleView ] [ text "Toggle" ], button [ onClick Play ] [ text "Play!" ], viewDemo model.hand model.castHistory ]

        Game game ->
            div [ style "background-color" "#F5F5F5", style "border-radius" "5px" ] [ viewGame game ]


viewGame : Game -> Html Msg
viewGame game =
    case game of
        Selection hand player enemy castHistory ->
            div []
                [ viewSelect hand player enemy castHistory
                , div [ id "history" ]
                    [ h2 [] [ text "History" ]
                    , div [] (List.map viewResolution castHistory)
                    ]
                ]

        Castable hand player enemy castHistory ->
            div []
                [ viewSelect hand player enemy castHistory
                , button [ onClick GameCast ] [ text "Cast" ]
                , div [ id "history" ]
                    [ h2 [] [ text "History" ]
                    , div [] (List.map viewResolution castHistory)
                    ]
                ]

        Done castHistory ->
            viewGameOver castHistory


viewSelect : List SelectableElement -> Int -> Int -> List Resolution -> Html Msg
viewSelect hand player enemy castHistory =
    div []
        [ viewGameStats player enemy castHistory
        , div [ style "display" "flex", style "width" "100%" ]
            [ div [ id "hand", style "display" "flex", style "flex-direction" "column", style "width" "50%", style "align-items" "center" ]
                [ h2 [] [ text "Hand" ]
                , div [ style "display" "flex", style "flex-direction" "row", style "align-items" "flex-end", style "height" "50px" ]
                    (List.indexedMap (\index se -> viewSelectableElement (GameToggleElement index) se) hand)
                ]
            , div [ id "on-deck", style "display" "flex", style "flex-direction" "column", style "width" "50%", style "align-items" "center" ]
                [ h2 [] [ text "On Deck" ]
                , div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "center" ]
                    (parseAsGrouping (hand |> List.filter (\(SelectableElement _ s) -> s) |> List.map (\(SelectableElement element _) -> element)))
                ]
            ]
        ]


viewGameOver : List Resolution -> Html Msg
viewGameOver castHistory =
    div []
        [ viewGameStats 10 0 castHistory
        , div [ id "history" ]
            [ h2 [] [ text "History" ]
            , div [] (List.map viewResolution castHistory)
            ]
        ]


viewDemo : List SelectableElement -> List Resolution -> Html Msg
viewDemo hand castHistory =
    div []
        [ div [ style "display" "flex", style "justify-content" "space-between" ]
            [ div []
                [ text <| String.fromInt <| length <| hand
                , button [ onClick Draw ] [ text "Draw" ]
                ]
            , div [ id "hand", style "display" "flex" ]
                [ h2 [] [ text "Hand now" ]
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
                    , text "Life: "
                    , text <| String.fromInt <| List.foldl (\(Resolution data) acc -> List.foldl (\(SpellConfiguration instance) iAcc -> iAcc + instance.life) acc data.results) 0 castHistory
                    ]
                , div [] (List.map viewResolution castHistory)
                ]
            ]
        ]


viewGameStats : Int -> Int -> List Resolution -> Html Msg
viewGameStats player enemy castHistory =
    let
        turns =
            length castHistory

        damage =
            List.foldl (\(Resolution data) acc -> List.foldl (\(SpellConfiguration instance) iAcc -> iAcc + instance.life) acc data.results) 0 castHistory
    in
    div [ id "game-info", style "display" "flex", style "justify-content" "space-between", style "flex-direction" "row" ]
        [ h2 [] [ text "Game Deets" ]
        , span [] [ text "Player: ", text <| String.fromInt player ]
        , span [] [ text "Enemy: ", text <| String.fromInt enemy ]
        , span [] [ text "Turns: ", text <| String.fromInt <| turns ]
        , span [] [ text "Damage: ", text <| String.fromInt <| damage ]
        ]


viewDiscover : List Element -> Html Msg
viewDiscover selected =
    div [ style "display" "flex", style "align-content" "flex-start" ]
        [ div [ id "spellbook" ]
            [ h2 [] [ text "Spellbook" ]
            , div [ style "display" "flex", style "flex-direction" "column" ] [ displaySpellbook ]
            ]
        , div [ id "selection", style "flex-grow" "2", style "margin-left" "10%" ]
            [ h2 [] [ text "Add" ]
            , div [ style "display" "flex" ]
                [ dot (Add Earth) Earth
                , dot (Add Fire) Fire
                , dot (Add Water) Water
                , dot (Add Wind) Wind
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


displayGrouping : SpellConfiguration -> Html Msg
displayGrouping (SpellConfiguration { name, elements }) =
    div [ style "background-color" "#CBC3E3", style "margin" "5px", style "border-radius" "5px", style "padding" "5px" ]
        [ span [ style "text-align" "center" ] [ text name ]
        , br [] []
        , span [] <| List.indexedMap (\index e -> dot (Remove index) e) elements
        ]


resolveElements : List Element -> Resolution
resolveElements elements =
    Resolution { elements = elements, results = foldElements elements }


{-| Find an entry in the spellbook, or treat each element individually
-}
foldElements : List Element -> List SpellConfiguration
foldElements elements =
    case castSpell elements of
        Just spell ->
            [ spellConfiguration spell ]

        Nothing ->
            elements
                |> List.map (\e -> castSpell [ e ])
                |> List.filterMap identity
                |> List.map spellConfiguration


parseAsGrouping : List Element -> List (Html Msg)
parseAsGrouping elements =
    elements
        |> foldElements
        |> List.map displayGrouping


castSpell : List Element -> Maybe Spell
castSpell elements =
    Dict.Any.get elements spellbook


viewResolution : Resolution -> Html Msg
viewResolution (Resolution data) =
    let
        viewSpellConfiguration : SpellConfiguration -> Html Msg
        viewSpellConfiguration (SpellConfiguration instance) =
            div [ style "display" "flex" ]
                [ displayGrouping (SpellConfiguration instance)
                , div [] [ text "Damage: ", text (String.fromInt instance.life) ]
                , div [] (text " Effects: " :: List.map (\e -> text e) instance.effects)
                ]
    in
    div [] (List.map viewSpellConfiguration data.results)


elementToString : Element -> String
elementToString element =
    case element of
        Fire ->
            "Fire"

        Water ->
            "Water"

        Earth ->
            "Earth"

        Wind ->
            "Wind"


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
            "green"

        Wind ->
            "grey"


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
