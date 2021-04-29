import Data.Maybe

data MayHaveValue a = NoValue | HasValue a deriving (Show)

data ToBeOrNot a = NotToBe | ToBe a deriving (Show)

data HasSomething a = HasNothing | HasSomething a deriving (Show)

class Maybish a where
    fromMaybe :: Maybe b -> a b
    toMaybe   :: a b -> Maybe b

instance Maybish MayHaveValue where
    fromMaybe   (Just b) = HasValue b
    fromMaybe   Nothing = NoValue

    toMaybe     (HasValue b) = Just b
    toMaybe     NoValue = Nothing

instance Maybish ToBeOrNot where
    fromMaybe   (Just b) = ToBe b
    fromMaybe   Nothing = NotToBe

    toMaybe     (ToBe b) = Just b
    toMaybe     NotToBe = Nothing

instance Maybish HasSomething where
    fromMaybe   (Just b) = HasSomething b
    fromMaybe   Nothing = HasNothing

    toMaybe     (HasSomething b) = Just b
    toMaybe     HasNothing = Nothing

instance Maybish Maybe where
    fromMaybe   b = b
    toMaybe     b = b