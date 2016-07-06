import ElmTest exposing (..)

import TodoItem exposing (tests)
import TodoEntry exposing (tests)

testSuite =
    suite "A Test Suite"
        (TodoItem.tests ++ TodoEntry.tests)


main = 
    runSuiteHtml testSuite
