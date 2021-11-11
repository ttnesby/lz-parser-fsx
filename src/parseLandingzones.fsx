#load @"./landingzones.fsx"

open LandingZoneYaml
open LandingZone
open ALogger

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml" 
|> LandingZoneYaml.tryList
|> Result.bind LandingZone.tryList
|> Result.map (fun l -> ALog.inf $"%A{l}")