#load @"./landingzones.fsx"

open LandingZoneYaml
open LandingZone
open ALogger

let resultAsExitCode = function | Error e -> ALog.err $"{e}"; 1 | Ok _ -> 0

// verify that tenant mnggrpid integrity

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml" 
|> LandingZoneYaml.tryList
|> Result.bind LandingZone.tryList
|> Result.bind LandingZone.uniqueNameAndType
// verify that new tech contact is in Azure AD
// verify that type of lz has not changed
// load current set of json-files for online and hybrid
// move decommissioned json-files to decommissioned
// update existing json-files
// generate new json-files
|> resultAsExitCode
|> fun ec ->
    ALog.inf $"Exit code: {ec}"
    exit ec