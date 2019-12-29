module Types.ProductCode(
  ProductCode(Widget, Gizmo), create, value -- exporting data constructors to be able to pattern match
) where

-- A ProductCode is either a Widget or a Gizmo
data ProductCode = 
  -- The codes for Widgets start with a "W" and then four digits
  Widget String | 
  -- The codes for Gizmos start with a "G" and then three digits. 
  Gizmo String deriving (Eq, Show)

create :: String -> ProductCode
create = Widget -- TODO for now only widget

value :: ProductCode -> String
value (Widget value) = value
value (Gizmo value) = value
