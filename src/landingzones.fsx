#load @"./landingzonesYaml.fsx"

namespace LandingZone

open LandingZoneYaml

type Environment = private {
    OfType: DTO.EnvType
    MonthlyLimit: uint32
}
type NamePattern = private NamePattern of string
with
    member this.Value = let (NamePattern s) = this in s

type EmailPattern = private EmailPattern of string
with
    member this.Value = let (EmailPattern s) = this in s

[<RequireQualifiedAccess>]
type Pattern =
    private
    | Name of np: NamePattern
    | Email of ep: EmailPattern

type Contacts = private {
    Team:       Pattern
    Technical:  Pattern
    Budget:     Pattern
}

type LandingZone = private {
    Name:           Pattern
    OfType:         DTO.LZType
    Environments:   Environment list
    Contacts:       Contacts
}

module Helpers =

    open System.Text.RegularExpressions

    let reNP = Regex("""^[a-z][a-z0-9-]{2,29}(?<!-)$""", RegexOptions.Compiled)
    let reEP = Regex("""^\S+@\S+\.\S+$""", RegexOptions.Compiled)

    let private isPatternCompliant (pat: Regex) fType str =
        let patErr () = $"[{str}] is not compliant with regex [{pat}]" |> Error
        let regErr err = $"regex [{pat}] failed for [{str}] - {err}" |> Error

        try if pat.Match(str).Success then str |> fType |> Ok else patErr()
        with | e -> regErr e.Message

    let isValidName = isPatternCompliant reNP NamePattern
    let isEmail = isPatternCompliant reEP EmailPattern

    let okToSome r = match r with | Ok x -> Some x | Error y -> None
    let errToSome r  = match r with | Ok x -> None | Error y -> Some y

    let listSeqRes (l: Result<'o,'e> list) =
        let okList = l |> (List.map okToSome >> List.choose id)
        let errList = l |> (List.map errToSome >> List.choose id)

        if List.isEmpty errList then Ok okList else Error errList

[<RequireQualifiedAccess>]
module Environment =
    let create (e: DTO.Environment) =
        if e.monthlyLimit >= 500u
        then Ok {Environment.OfType = e.OfType; MonthlyLimit = e.monthlyLimit}
        else $"MonthlyLimit {e.monthlyLimit} < 500 as the lowest limit" |> Error

[<RequireQualifiedAccess>]
module NamePattern =
    let create s = Helpers.isValidName s

[<RequireQualifiedAccess>]
module EmailPattern =
    let create s = Helpers.isEmail s

[<RequireQualifiedAccess>]
module Pattern =
    let asName = NamePattern.create >> Result.map Pattern.Name
    let asEmail = EmailPattern.create >> Result.map Pattern.Email

[<RequireQualifiedAccess>]
module Contacts =
    let create (c: DTO.Contacts) =
        [Pattern.asName c.Team; Pattern.asEmail c.Technical; Pattern.asEmail c.Budget ]
        |> Helpers.listSeqRes
        |> function
        | Ok [team; tech; bud] -> Ok {Contacts.Team = team; Technical = tech; Budget = bud }
        | Ok _ -> Error ["Should never happen, but incomplet pattern matching is gone"]
        | Error el -> Error el

[<RequireQualifiedAccess>]
module LandingZone =

    open ALogger
    open System.Text

    let private envSize l =
        if List.length l >= 1 && List.length l <=3 then Ok l else Error ["0 or more than 3 env."]

    let private envUnique (l: DTO.Environment list) =
        if List.length l = (l |> List.map (fun e -> e.OfType) |> List.distinct |> List.length)
        then Ok l else Error ["env. duplications"]

    let uniqueNameAndType (l: LandingZone list) = 
        let mapper lz = (lz.Name,lz.OfType) 
        ALog.inf "Checking (lzname, type) duplicates in list"
        if List.length l = (l |> List.map mapper |> List.distinct |> List.length)
        then Ok l 
        else
            l |> List.map mapper |> List.groupBy id 
            |> List.filter (fun (_, l) -> List.length l > 1) 
            |> List.map (fun (k,_) -> 
                match fst k with | Pattern.Name np -> (np.Value,snd k) | Pattern.Email ep -> (ep.Value, snd k)
                )
            |> fun dup ->  Error $"duplicates of %A{dup} in list of landing zones"


    let create (lz: DTO.LandingZone) = async {
        let context = $"Landing zone [lzname: {lz.Name}, type: {lz.OfType}] errors:"

        let logAndError = ALog.logPassThroughStr ALog.err >> Error
        let appender (acc: StringBuilder) (s:string) = acc.AppendLine(s)
        let allStrings l = l |> List.fold appender (StringBuilder().AppendLine(context)) |> fun sb -> sb.ToString()

        let name = Pattern.asName lz.Name |> Result.mapError (fun e -> [e])
        let envs = envSize lz.Environments
                    |> Result.bind envUnique
                    |> Result.bind (List.map Environment.create >> Helpers.listSeqRes)

        let con = Contacts.create lz.Contacts

        let errList = [Helpers.errToSome name; Helpers.errToSome envs; Helpers.errToSome con]
                        |> List.choose id
                        |> List.concat
        return
            (name, envs, con)
            |> function
            | Ok name, Ok envs, Ok con ->
                Ok {LandingZone.Name = name; OfType = lz.OfType; Environments = envs; Contacts = con }
            | _ -> errList |> allStrings |> logAndError
    }

    let tryList (l: DTO.LandingZone list) =

        let listSeqRes (l: Result<'o,'e> list) =
            let okList = l |> (List.map Helpers.okToSome >> List.choose id)
            let errList = l |> (List.map Helpers.errToSome >> List.choose id)

            if List.isEmpty errList then Ok okList else Error ""

        let logAndCreate (lz: DTO.LandingZone) =
            lz |> ALog.logPassThroughX ALog.inf $"Checking [lzname: {lz.Name}, type: {lz.OfType}]" |> create

        (List.map logAndCreate >> Async.Parallel >> Async.RunSynchronously >> Array.toList >> listSeqRes) l