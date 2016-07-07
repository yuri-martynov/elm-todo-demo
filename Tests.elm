module Main exposing (..)

import ElmTest exposing (..)
import EditableInput exposing (tests)
import TodoEntry exposing (tests)
import Todo exposing (tests)


testSuite =
    suite "A Test Suite"
        (EditableInput.tests
            ++ TodoEntry.tests
            ++ Todo.tests
        )


main =
    runSuiteHtml testSuite
