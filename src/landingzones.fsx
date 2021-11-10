#load @"./../.paket/load/Legivel.fsx"
#load @"./utils/aLogger.fsx"

//namespace LandingZones

type LZName = LZName of string

type LZType =
| Online
| Hybrid

type EnvType = 
| Dev
| Test
| Prod

type Environment = {
    OfType: EnvType
    monthlyLimit: uint32
}

type TeamName = TeamName of string
type EMail = EMail of string

type Contacts = {
    Team: TeamName
    Technical: EMail
    Budget: EMail
}

type LandingZone = {
    Name: LZName
    OfType: LZType
    Environments: seq<Environment>    
    Contacts: Contacts
}

type LandingZones = seq<LandingZone>

type GetYamlFile = unit -> Result<LandingZones, string>

module LandingZones =

    open ALogger

    module Helpers =

        let errToOption (r: Result<'o,'e>) =  match r with | Error _ -> Some true | Ok _ -> None

        let readFile (filePath: string) =
            try
                filePath
                |> fun f -> ALog.inf $"Reading yaml [{f}]"; f
                |> System.IO.File.ReadAllText
                |> Ok
            with
            | e ->
                $"Cannot read {filePath} - {e.Message}"
                |> ALog.logPassThroughStr ALog.err
                |> Error

    let private extractYamlData (r: Legivel.Serialization.DeserializeResult<LandingZones> list) :
        Result<LandingZones, string> =
            match r with
            // expecting only 1 yaml doc
            | [h:_] when List.length r = 1 ->
                match h with
                | Legivel.Serialization.DeserializeResult.Success d ->
                    ALog.inf "Yaml file parsing ok"
                    Ok d.Data
                | Legivel.Serialization.DeserializeResult.Error e ->
                    $"Yaml file parsing failure - %A{e}"
                    |> ALog.logPassThroughStr ALog.err
                    |> Error
            | _ ->
                "Yaml parsing - Either none or too many yaml documents found"
                |> ALog.logPassThroughStr ALog.err
                |> Error

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml"
|> LandingZones.Helpers.readFile
|> Result.map Legivel.Serialization.Deserialize<LandingZones>