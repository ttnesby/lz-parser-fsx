#load @"./../.paket/load/Legivel.fsx"
#load @"./utils/aLogger.fsx"

namespace LandingZoneYaml

open Legivel.Attributes

module DTO = 

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

type TryList = string -> Result<DTO.LandingZone list, string>

module LandingZoneYaml =

    open ALogger
    open DTO

    type YamlType = 
    | [<YamlValue("landingzones")>] LandingZones = 0

    type LandingZones = Map<YamlType,LandingZone list>

    let private readFile (filePath: string) =
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

    let private extractYamlData (r: Legivel.Serialization.DeserializeResult<LandingZones> list) =
            match r with
            // expecting only 1 yaml doc
            | [h] ->
                match h with
                | Legivel.Serialization.DeserializeResult.Success d ->
                    ALog.inf "Reading completed - ok"
                    Ok d.Data[YamlType.LandingZones]
                | Legivel.Serialization.DeserializeResult.Error e ->
                    match e.Error with
                    | [h:_] -> $"Yaml failure - approx. @line {h.Location.Line} - {h.Message}"
                    | _ -> $"Yaml failure - %A{e}"                    
                    |> ALog.logPassThroughStr ALog.err
                    |> Error
            // in case of void yaml doc 
            | [] -> List.empty<LandingZone> |> Ok
            // in case of multiple yaml docs
            | _ ->
                $"Yaml failure, expected just one yaml doc - found {List.length r} documents"
                |> ALog.logPassThroughStr ALog.err
                |> Error

    let tryList : TryList =  
        readFile >> Result.map Legivel.Serialization.Deserialize<LandingZones> >> Result.bind extractYamlData