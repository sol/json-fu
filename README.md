# json-fu: JSON serialization / deserialization

## Requires

~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.ByteString (ByteString)
import Data.JSON
~~~

## Converts fields into `snake_case`

~~~ {.haskell}
data Person = Person {
  firstName :: String
, lastName  :: String
, age       :: Int
} deriving (Eq, Show, Data, Typeable)

person :: ByteString
person = toJSON (Person "John" "Doe" 23)
~~~

This will resutl in JSON that is equivalent to:
```json
{
  "first_name":"John",
  "last_name": "Doe",
  "age": 23
}
```

`fromJSON person :: Maybe Person` can be used to recover the original data
type.

## Strips constructor name from fields

~~~ {.haskell}
data Message = Message {
  messageBody     :: String
, messageBodySize :: Int
} deriving (Eq, Show, Data, Typeable)

message :: ByteString
message = toJSON (Message "foobar" 23)
~~~

This will resutl in JSON that is equivalent to:
```json
{
  "body": "foobar",
  "body_size":23
}
```

`fromJSON message :: Maybe Message` can be used to recover the original data
type.
