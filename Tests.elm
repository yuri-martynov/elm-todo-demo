import ElmTest exposing (..)

import TodoItem exposing (tests)

all : Test
all =
    suite "A Test Suite"
        TodoItem.tests

main = 
    ElmTest.runSuiteHtml all