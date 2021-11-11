#load @"./landingzones.fsx"

open LandingZoneYaml
open LandingZone
open ALogger

let resultAsExitCode = function | Error e -> ALog.err $"{e}"; 1 | Ok _ -> 0

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml" 
|> LandingZoneYaml.tryList
|> Result.bind LandingZone.tryList
|> resultAsExitCode
|> fun ec ->
    ALog.inf $"Exit code: {ec}"
    exit ec