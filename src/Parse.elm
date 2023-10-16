module Parse exposing (parse)

import Elm.Parser exposing (parseToFile)
import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)


parse : String -> Result (List DeadEnd) File
parse inputs =
    parseToFile (String.trim inputs)
