import ElmTest exposing (..)

import TodoEntry exposing (tests)

testSuite =
    suite "A Test Suite"
        TodoEntry.tests

main = 
    runSuiteHtml testSuite