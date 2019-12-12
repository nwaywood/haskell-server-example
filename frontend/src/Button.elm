module Button exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input



-- default button styling supporting override (last instance of a style in the list will be used)


button l r =
    Input.button ([ Border.rounded 3, Border.color <| rgb255 200 200 200, Border.width 1 ] ++ l) r
