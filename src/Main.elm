port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task


port store : Model -> Cmd msg


storeWithUpdate : Msg -> Model -> ( Model, Cmd Msg )
storeWithUpdate msg model =
    let
        ( newModel, cmds ) =
            update msg model

        message =
            if List.length (List.filter (\todoTask -> not todoTask.done) model.tasks) < 2 then
                "Welcome back, feel free to add some new tasks"

            else
                "Welcome back, don't forget to finish your tasks before adding new ones"
    in
    ( newModel
    , Cmd.batch [ store { newModel | errorMessage = message, doneTasksVisibility = False, inputFieldText = "" }, cmds ]
    )


type alias Task =
    { name : String
    , done : Bool
    , id : Int
    , editing : Bool
    }


type alias Model =
    { tasks : List Task
    , inputFieldText : String
    , errorMessage : String
    , idGenerator : Int
    , doneTasksVisibility : Bool
    }


emptyModel : Model
emptyModel =
    { tasks = []
    , inputFieldText = ""
    , errorMessage = "Welcome \n Write Tasks, Not Goals"
    , idGenerator = 0
    , doneTasksVisibility = False
    }


type Msg
    = AddTask
    | ChagedInputText String
    | Select Int String Bool
    | EditMode Int Bool
    | UpdateTask Int String
    | Delete Int
    | Nop
    | ChangeVisibility
    | DeleteDoneTasks


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [ class "todo-app" ]
        [ h1 [ class "message" ] [ text model.errorMessage ]
        , div [ class "todo-form" ]
            [ input
                [ placeholder "What's the Plan for Today"
                , onEnter AddTask
                , autocomplete False
                , value model.inputFieldText
                , class "todo-input"
                , id "iField"
                , onInput ChagedInputText
                ]
                []
            , button [ onClick AddTask, class "todo-button" ] [ text "Add task" ]
            , div [ class "todo-list" ] <|
                List.map (\todoTask -> displayTask todoTask) <|
                    List.filter (\todoTask -> not todoTask.done) model.tasks
            , if (List.length <| List.filter (\todoTask -> todoTask.done) model.tasks) > 0 then
                div [ class "done", onClick ChangeVisibility, onDoubleClick Nop ]
                    [ label [] [ text "Done" ]
                    , img
                        [ src "checkmark.png"
                        , width 30
                        , height 30
                        ]
                        []
                    , img
                        [ src "bin.png"
                        , width 30
                        , height 30
                        , onClick DeleteDoneTasks
                        ]
                        []
                    , if model.doneTasksVisibility then
                        img
                            [ src "up.png"
                            , width 30
                            , height 30
                            ]
                            []

                      else
                        img
                            [ src "down.png"
                            , width 30
                            , height 30
                            ]
                            []
                    ]

              else
                label [] []
            , if model.doneTasksVisibility then
                div [ class "done-list" ] <|
                    List.map (\todoTask -> displayTask todoTask) <|
                        List.filter (\todoTask -> todoTask.done) model.tasks

              else
                span [] []
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask ->
            let
                validTask =
                    ( { model
                        | idGenerator = model.idGenerator + 1
                        , tasks = model.tasks ++ [ Task model.inputFieldText False model.idGenerator False ]
                        , inputFieldText = ""
                        , errorMessage = "New Task added successfully"
                      }
                    , Cmd.none
                    )

                isOneTheList =
                    List.isEmpty <| List.filter (\task -> task.name == model.inputFieldText && task.done == False) model.tasks
            in
            if (String.length model.inputFieldText < 3) && isOneTheList then
                ( { model | errorMessage = "Name of the task must be longer than 3 characters" }, Cmd.none )

            else if not <| isOneTheList then
                ( { model | errorMessage = "This task is already on the list" }, Cmd.none )

            else
                validTask

        ChagedInputText typedText ->
            ( { model | inputFieldText = typedText }, Cmd.none )

        Select taskId name state ->
            let
                updateTask task =
                    if task.id == taskId then
                        { task | done = not task.done }

                    else
                        task

                message =
                    if state then
                        "Task " ++ name ++ " marked as uncomplete"

                    else
                        "Task " ++ name ++ " marked as complete"
            in
            ( { model | tasks = List.map updateTask model.tasks, errorMessage = message }, Cmd.none )

        Delete taskId ->
            ( { model | tasks = List.filter (\task -> task.id /= taskId) model.tasks, errorMessage = "Task deleted successfully" }, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        EditMode id isEditing ->
            let
                updateTask task =
                    if task.id == id then
                        { task | editing = isEditing }

                    else
                        task
            in
            if isEditing then
                ( { model | tasks = List.map updateTask model.tasks, errorMessage = "Press enter to confirm the change" }
                , Task.attempt (\_ -> Nop) <| Dom.focus ("editField-" ++ String.fromInt id)
                )

            else
                ( { model | tasks = List.map updateTask model.tasks, errorMessage = "Task updated successfully" }
                , Task.attempt (\_ -> Nop) <| Dom.focus "iField"
                )

        UpdateTask id taskName ->
            let
                updateTask : Task -> Task
                updateTask task =
                    if task.id == id then
                        { task | name = taskName }

                    else
                        task
            in
            ( { model | tasks = List.map updateTask model.tasks }
            , Cmd.none
            )

        ChangeVisibility ->
            ( { model | doneTasksVisibility = not model.doneTasksVisibility }, Cmd.none )

        DeleteDoneTasks ->
            ( { model | tasks = List.filter (\todoTask -> not todoTask.done) model.tasks, errorMessage = "All completed tasks are successfully deleted" }, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = storeWithUpdate
        , subscriptions = subscriptions
        }


checkbox : Task -> Html Msg
checkbox task =
    let
        message =
            Select task.id
    in
    input
        [ type_ "checkbox"
        , class "todo-checkbox"
        , checked task.done
        , onClick (Select task.id task.name task.done)
        ]
        []


deleteButton : Task -> Html Msg
deleteButton task =
    let
        deleteTaskHelp =
            Delete task.id
    in
    img
        [ src "d-button.png"
        , width 20
        , height 20
        , onClick deleteTaskHelp
        ]
        []


editButton : Task -> Html Msg
editButton task =
    let
        editTaskHelp =
            EditMode task.id True
    in
    img
        [ src "edit.png"
        , width 20
        , height 20
        , onClick editTaskHelp
        ]
        []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not-ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


displayTask : Task -> Html Msg
displayTask task =
    div
        [ classList [ ( "editing", task.editing ), ( "complete", task.done ) ], class "todo-row", onDoubleClick (Select task.id task.name task.done) ]
        [ div
            [ class "task" ]
            [ checkbox task
            , label
                []
                [ text task.name ]
            , div [ class "icons" ]
                [ deleteButton task
                , if task.done then
                    span [] []

                  else
                    editButton task
                ]
            ]
        , input
            [ class "editField"
            , value task.name
            , id ("editField-" ++ String.fromInt task.id)
            , onEnter (EditMode task.id False)
            , onInput (UpdateTask task.id)
            , onBlur (EditMode task.id False)
            ]
            []
        , button
            [ onClick <| EditMode task.id False
            , class "update-button"
            ]
            [ text "Update" ]
        ]
