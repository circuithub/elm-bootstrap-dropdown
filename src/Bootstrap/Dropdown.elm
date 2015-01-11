module Bootstrap.Html where
{-| Dropdown widgets for Bootstrap.

## Button dropdowns
* Single button dropdowns
@docs buttonDropdown'

* Split button dropdowns (TODO)
* Sizing (TODO)
* Dropup variation (TODO)

-}

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Shorthand (..)
import String
import List
import Signal
import Signal (Signal)

-- Button dropdowns
buttonDropdown' : TextString -> Signal Html
buttonDropdown' buttonLabel =
  let -- Signal helpers
      map1 : (a -> b -> c) -> Signal a -> b -> Signal c
      map1 f x y = let f' x' = f x' y
                   in f' `Signal.map` x

      appendWithSpace : String -> String -> String
      appendWithSpace x y = x ++ ' ' `String.cons` y

      mapAppendClass : (ClassString -> b -> Html) -> ClassString -> Signal ClassString -> b -> Signal Html
      mapAppendClass f c sc = f `map1` (Signal.map (appendWithSpace c) sc)

      -- Send 'True' to the 'dropdownState' channel in order to open the menu or 'False' to close it
      -- TODO: The channel should probably be exposed to the user in order to provide external control
      --       over the state of the dropdown and read the current state of the dropdown
      dropdownState   = Signal.channel False
      stateFromBool b = if b then "open" else ""
  in  -- Button group has the "open" class when the dropdown is opened
      -- TODO: How should we close the dropdown if a click occurs outside of the menu
      mapAppendClass divc "btn-group" (Signal.map stateFromBool <| Signal.subscribe dropdownState)
      [ -- The dropdown button itself
        button
        [ class "btn btn-default dropdown-toggle"
        , type' "button"
        , onClick (Signal.send dropdownState True)
        ]
        [ text buttonLabel
        , spanc "caret" []
        ]
      , -- The dropdown menu (shown only when the parent has the 'open' class)
        ul
        [ class "dropdown-menu"
        , attribute "role" "menu"
        ]
        [ li []
          [ a
            [ attribute "role" "menuitem"
            , tabindex -1
            , href "#"
            -- TODO: onClick, two actions need to take place
            --       * the menu should be closed
            --       * and then a user-provided action should be triggered
            ]
            [ text "Action"
            ]
          ]
        ]
      ]
