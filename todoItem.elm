module TodoItem exposing (Model, Msg)

type alias Model = 
    { description : String
    , isDone : Bool
    , newDescription : Maybe String
    }

type Msg
    = NoOp
    | Done 
    | StartEditing 
    | FinishEditing 
    | TaskChanged String 
    | CancelEditing 
