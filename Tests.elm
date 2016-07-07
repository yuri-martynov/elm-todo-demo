import ElmTest exposing (..)

import EditableInput exposing (tests)
import TodoEntry exposing (tests)

testSuite =
    suite "A Test Suite"
        (EditableInput.tests ++ TodoEntry.tests)


main = 
    runSuiteHtml testSuite
