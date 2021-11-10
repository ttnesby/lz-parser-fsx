#load @"./../.paket/load/Legivel.fsx"
#load @"./utils/aLogger.fsx"

//namespace LandingZones

open Legivel.Attributes
open ALogger

//type LZName = LZName of string

type LZType =
| [<YamlValue("online")>] Online
| [<YamlValue("hybrid")>] Hybrid

type EnvType =
| [<YamlValue("dev")>] Dev
| [<YamlValue("test")>] Test
| [<YamlValue("prod")>] Prod

type Environment = {
    [<YamlField("name")>] OfType: EnvType
    monthlyLimit: uint32
}

//type TeamName = TeamName of string
//type EMail = EMail of string

type Contacts = {
    [<YamlField("team")>]       Team: string
    [<YamlField("technical")>]  Technical: string
    [<YamlField("budget")>]     Budget: string
}

type LandingZone = {
    [<YamlField("lzname")>]         Name: string
    [<YamlField("type")>]           OfType: LZType
    [<YamlField("environments")>]   Environments: Environment list
    [<YamlField("contacts")>]       Contacts: Contacts
}

type YamlType = 
| [<YamlValue("landingzones")>] LandingZones = 0

type LandingZones = Map<YamlType,LandingZone list>

module LandingZones =

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

    let extractYamlData (r: Legivel.Serialization.DeserializeResult<LandingZones> list) =
            match r with
            // expecting only 1 yaml doc
            | [h] ->
                match h with
                | Legivel.Serialization.DeserializeResult.Success d ->
                    ALog.inf "Yaml file reading completed - ok"
                    Ok d.Data[YamlType.LandingZones]
                | Legivel.Serialization.DeserializeResult.Error e ->
                    ALog.err $"%A{e}"
                    $"Yaml failure - approx. @line {e.Error.Head.Location.Line} - {e.Error.Head.Message}"
                    |> ALog.logPassThroughStr ALog.err
                    |> Error
            // in case of void yaml doc 
            | [] -> List.empty<LandingZone> |> Ok
            // in case of multiple yaml docs
            | _ ->
                $"Yaml failure, expected just one yaml doc - found {List.length r} documents"
                |> ALog.logPassThroughStr ALog.err
                |> Error

$"{__SOURCE_DIRECTORY__}/../landingzones.yaml"
|> LandingZones.Helpers.readFile
|> Result.map Legivel.Serialization.Deserialize<LandingZones>
|> Result.bind LandingZones.extractYamlData
|> Result.map (fun l -> ALog.inf $"%A{l}")
