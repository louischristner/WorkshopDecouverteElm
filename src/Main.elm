module Main exposing (main)

import List
import Browser
import String
import Http
import Json.Decode as JD
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Ingredient =
    { name : String
    , clicked : Bool
    }

type alias IngredientList =
    { viandes : List Ingredient
    , legumes : List Ingredient
    , supplements : List Ingredient
    , sauces : List Ingredient
    }

type alias Model =
    { ingredientList : IngredientList
    , tacos : List Ingredient
    , error : String
    }

initIngredientList : IngredientList
initIngredientList =
    { viandes = []
    , legumes = []
    , supplements = []
    , sauces = []
    }

initModel : Model
initModel =
    { ingredientList = initIngredientList
    , tacos = []
    , error = ""
    }

init : () -> (Model, Cmd Msg)
init _ = (initModel, getIngredientList)




type Msg = GotIngredientList (Result Http.Error IngredientList) | AddIngredient Ingredient

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotIngredientList result ->
            case result of
                Err err -> ({ model | error = Debug.toString err }, Cmd.none)
                Ok value ->
                    ({ model | ingredientList = value }, Cmd.none)

        AddIngredient ingredient ->
            let
                newIngredientList =
                    { viandes = List.map (\ing -> if ing == ingredient then { ing | clicked = not ing.clicked } else ing) model.ingredientList.viandes
                    , legumes = List.map (\ing -> if ing == ingredient then { ing | clicked = not ing.clicked } else ing) model.ingredientList.legumes
                    , supplements = List.map (\ing -> if ing == ingredient then { ing | clicked = not ing.clicked } else ing) model.ingredientList.supplements
                    , sauces = List.map (\ing -> if ing == ingredient then { ing | clicked = not ing.clicked } else ing) model.ingredientList.sauces
                    }

                newTacos = List.filter (\ing -> ing.clicked) newIngredientList.viandes ++ List.filter (\ing -> ing.clicked) newIngredientList.legumes ++ List.filter (\ing -> ing.clicked) newIngredientList.supplements ++ List.filter (\ing -> ing.clicked) newIngredientList.sauces
            in
                ({ model | ingredientList = newIngredientList, tacos = newTacos }, Cmd.none)



viewIngredient : Ingredient -> Html Msg
viewIngredient ingredient =
    li (if ingredient.clicked == True then [ style "background" "blue" ] else [])
        [ button [ onClick (AddIngredient ingredient) ]
            [ text ingredient.name ]
        ]

view model =
    div []
        [ h4 [] [ text "Viandes" ]
        , ul [] (List.map viewIngredient model.ingredientList.viandes)
        , h4 [] [ text "Légumes" ]
        , ul [] (List.map viewIngredient model.ingredientList.legumes)
        , h4 [] [ text "Suppléments" ]
        , ul [] (List.map viewIngredient model.ingredientList.supplements)
        , h4 [] [ text "Sauces" ]
        , ul [] (List.map viewIngredient model.ingredientList.sauces)
        , h4 [] [ text "Mon tacos" ]
        , ul [] (List.map viewIngredient model.tacos)
        ]





subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



ingredientDecoder : JD.Decoder Ingredient
ingredientDecoder =
    JD.map2 Ingredient
        JD.string
        (JD.succeed False)

ingredientListDecoder : JD.Decoder IngredientList
ingredientListDecoder =
    JD.map4 IngredientList
        (JD.field "viandes" (JD.list ingredientDecoder))
        (JD.field "legumes" (JD.list ingredientDecoder))
        (JD.field "supplements" (JD.list ingredientDecoder))
        (JD.field "sauces" (JD.list ingredientDecoder))

getIngredientList : Cmd Msg
getIngredientList =
    Http.get
        { url = "https://elmchatworkshop.osc-fr1.scalingo.io/ingredients"
        , expect = Http.expectJson GotIngredientList ingredientListDecoder
        }
