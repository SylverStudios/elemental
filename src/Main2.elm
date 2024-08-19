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
        { castHistory : List Resolution
        , gameState : GameState
        }


type alias Amount =
    Int


type alias TurnsRemaining =
    Int


type SpellEffect
    = Regen Amount TurnsRemaining
    | Amplify Amount


type Spellbook
    = Spellbook (AnyDict (List Int) (List Element) Spell)


type alias GameData =
    { planetLife : Int
    , planetEffects : List SpellEffect
    , turn : Int
    , spellbook : Spellbook
    , elements : List SelectableElement
    }


type GameState
    = GameState GameData


type Resolution
    = Resolution
        { elements : List Element
        , results : SpellConfiguration
        }


type SpellConfiguration
    = SpellConfiguration
        { elements : List Element
        , spell : Spell
        , life : Int
        , effects : List String
        , name : String
        }


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
        { castHistory = []
        , gameState =
            GameState
                { planetLife = 0
                , planetEffects = []
                , turn = 0
                , spellbook = Spellbook spellbook
                , elements = [ SelectableElement Fire False, SelectableElement Water False, SelectableElement Earth False ]
                }
        }



-- UPDATE


type Msg
    = GameToggleElement Int


update : Msg -> Model -> Model
update msg (Model model) =
    let
        (GameState gameData) =
            model.gameState

        toggleElementAtIndex : Int -> List SelectableElement -> List SelectableElement
        toggleElementAtIndex index =
            List.indexedMap
                (\idx (SelectableElement el isSelected) ->
                    if idx == index then
                        SelectableElement el (not isSelected)

                    else
                        SelectableElement el isSelected
                )
    in
    case msg of
        GameToggleElement index ->
            Model { model | gameState = GameState { gameData | elements = toggleElementAtIndex index gameData.elements } }


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


displayGrouping : SpellConfiguration -> Html Msg
displayGrouping (SpellConfiguration { name, elements }) =
    div [ style "background-color" "#CBC3E3", style "margin" "5px", style "border-radius" "5px", style "padding" "5px" ]
        [ span [ style "text-align" "center" ] [ text name ]
        , br [] []
        , span [] <| List.indexedMap (\index e -> dot (GameToggleElement index) e) elements
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
view (Model { gameState }) =
    case gameState of
        GameState gameData ->
            div [] [ viewState gameData, viewActions gameData ]


viewActions : GameData -> Html Msg
viewActions gameData =
    div [] (List.indexedMap (\idx se -> viewSelectableElement (GameToggleElement idx) se) gameData.elements)


viewState : GameData -> Html Msg
viewState gameData =
    let
        spellEffectToString : SpellEffect -> String
        spellEffectToString spellEffect =
            case spellEffect of
                Regen amount turns ->
                    "Regen:" ++ String.fromInt amount ++ String.fromInt turns

                Amplify amount ->
                    "Amplify:" ++ String.fromInt amount

        joinSpellEffects : List SpellEffect -> String
        joinSpellEffects se =
            se |> List.map spellEffectToString |> List.foldl (++) ""

        selectableElementToString : SelectableElement -> String
        selectableElementToString (SelectableElement element isSelected) =
            if isSelected then
                " ("
                    ++ elementToString element
                    ++ ") "

            else
                " "
                    ++ elementToString element
                    ++ " "
    in
    div []
        [ div [] [ text "Planet Life: ", text (String.fromInt gameData.planetLife) ]
        , div [] [ text "Planet Effects: ", text (joinSpellEffects gameData.planetEffects) ]
        , div [] [ text "Turn: ", text (String.fromInt gameData.turn) ]
        , div [] [ text "Elements: ", text (gameData.elements |> List.map selectableElementToString |> List.foldl (++) "") ]
        ]


castSpell : List Element -> Maybe Spell
castSpell elements =
    Dict.Any.get elements spellbook


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
