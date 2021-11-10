#load @"./landingzonesYaml.fsx"

open LandingZoneYaml
open ALogger

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml" 
|> LandingZoneYaml.tryList
|> Result.map (fun l -> ALog.inf $"%A{l}")