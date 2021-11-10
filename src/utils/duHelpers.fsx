namespace DU

[<RequireQualifiedAccess>]
module Utils =

    open Microsoft.FSharp.Reflection
    open FSharp.Collections

    let noOfTypes<'du> () = FSharpType.GetUnionCases typeof<'du> |> Array.length

    let toStringList<'du> () =
        FSharpType.GetUnionCases typeof<'du>
        |> Array.toList
        |> List.map (fun e -> e.Name)

    let toString<'du> x =
        FSharpValue.GetUnionFields(x, typeof<'du>)
        |> function
            | case, _ -> case.Name

    let fromString<'du> (s: string) =
        FSharpType.GetUnionCases typeof<'du>
        |> Array.filter (fun case -> case.Name.ToLower() = s)
        |> function
            | [| case |] -> FSharpValue.MakeUnion(case, [||]) :?> 'du |> Ok
            | _ ->
                $"Cannot find {typeof<'du>} for {s}"
                |> Error

    let asArray<'du> () =
        FSharpType.GetUnionCases typeof<'du>
        |> Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> 'du )
