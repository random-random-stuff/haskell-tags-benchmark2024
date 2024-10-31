{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Csv

data Person = Person { name :: !Text, age :: !Int }

instance ToNamedRecord Person where
    toNamedRecord (Person name age) = namedRecord [
        "name" .= name, "age" .= age]
