module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h2, p, span, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Main2 exposing (Model)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


type Inventory
    = Inventory
        { cookedFood : Int
        , clothes : Int
        , armor : Int
        , weapon : Int
        , bandage : Int
        , rawFood : Int
        , rawCloth : Int
        }



-- type Skill
--     = Cook
--     | Grow
--     | Sew


type Trait
    = Hungry
    | Haggard
    | Dishevled


type Visitor
    = Warrior Trait
    | Settler Trait
    | Merchant Trait


type Phase
    = Simulate (List Visitor)
    | Prepare


type Model
    = Model
        { day : Int
        , phase : Phase
        , player : { inventory : Inventory, cash : Int }
        , todd : { inventory : Inventory }
        }


init : Model
init =
    Model
        { day = 0
        , phase = Prepare
        , player =
            { inventory =
                Inventory
                    { cookedFood = 2
                    , clothes = 1
                    , armor = 0
                    , weapon = 0
                    , bandage = 1
                    , rawFood = 0
                    , rawCloth = 0
                    }
            , cash = 10
            }
        , todd =
            { inventory =
                Inventory
                    { cookedFood = 0
                    , clothes = 0
                    , armor = 0
                    , weapon = 0
                    , bandage = 0
                    , rawFood = 0
                    , rawCloth = 0
                    }
            }
        }


type Msg
    = Nope


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        Nope ->
            Model model


view : Model -> Html Msg
view (Model model) =
    div [] [ button [ onClick Nope ] [], text (String.fromInt model.player.cash) ]
