module Types.Utils where

-- Helper function to get the value from an Option, and if None, use the defaultValue
defaultIfNone :: a -> Maybe a -> a
defaultIfNone defaultValue (Just v) = v
defaultIfNone defaultValue Nothing = defaultValue
