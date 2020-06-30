module Runner exposing (..)

runnerUrl : Bool -> String
runnerUrl runnerRunning =
    case runnerRunning of
        True -> desktopRunnerUrl
        False -> webRunnerUrl

desktopRunnerUrl : String
desktopRunnerUrl =
    "http://127.0.0.1:37465"

webRunnerUrl : String
webRunnerUrl =
    ""
