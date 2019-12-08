module Types.ProductCode(
  ProductCode(Widget, Gizmo), create, value -- exporting data constructors to be able to pattern match
) where

data ProductCode = Widget String | Gizmo String deriving (Eq, Show)

create :: String -> ProductCode
create = Widget -- TODO for now only widget

value :: ProductCode -> String
value (Widget value) = value
value (Gizmo value) = value
