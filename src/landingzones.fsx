#load @"./../.paket/load/Legivel.fsx"
#load @"./utils/aLogger.fsx"

namespace LandingZone

open System.Text.RegularExpressions

type LZType =
| Online
| Hybrid

type EnvType =
| Dev
| Test
| Prod

type Environment = private {
    OfType: EnvType
    MonthlyLimit: uint32
}
type NamePattern = private NamePattern of string with
    member this.Value = let (NamePattern s) = this in s

type EmailPattern = private EmailPattern of string with
    member this.Value = let (EmailPattern s) = this in s

type Contacts = private {
    Team: NamePattern
    Technical: EmailPattern
    Budget: EmailPattern
}

type LandingZone = private {
    Name: NamePattern
    OfType: LZType
    Environments: Environment list
    Contacts: Contacts
}

module Helpers = 
    let private namePattern = """^[a-z][a-z0-9-]{2,29}(?<!-)$"""
    let private emailPattern = """^\S+@\S+\.\S+$"""

    let private isPatternCompliant s p = Regex.Match(s, p).Success

    let isValidName = isPatternCompliant namePattern
    let isEmail = isPatternCompliant emailPattern

module Environment = 
    let create (ofType, ml) = 
        if ml >= 500u 
        then Ok {Environment.OfType = ofType; MonthlyLimit = ml}
        else Error $"MonthlyLimit {ml} < 500 as the lowest limit"

module NamePattern = 
    let create s = 
        if Helpers.isValidName s then s |> NamePattern |> Ok else Error $"{s} is invalid name"

module EmailPattern = 
    let create s = 
        if Helpers.isValidName s then s |> EmailPattern |> Ok else Error $"{s} is invalid email"        

module Contacts = 
    let create (team, tech, bud) = 
        let r = (NamePattern.create team, EmailPattern.create tech, EmailPattern.create bud)
        match r with
        | Ok team, Ok tech, Ok bud -> Ok {Contacts.Team = team; Technical = tech; Budget = bud }
        | _ -> Error $"{team}, {tech} or {bud} has invalid name - or email pattern"

// module LandingZone = 
//     let create (name, ofType, envList, contacts) = 
