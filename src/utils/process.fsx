#load @"./aLogger.fsx"

namespace Proc

type Name = Name of string with
    member this.Value = let (Name s) = this in s

type TimeoutInSec = private TimeoutInSec of int with
    member this.Value = let (TimeoutInSec s) = this in s
type Arguments = Arguments of string with
    member this.Value = let (Arguments s) = this in s

type StandardOutput = StandardOutput of string with
    member this.Value = let (StandardOutput s) = this in s

type ErrorType =
    | InvalidTimeoutError of e:string
    | TimeoutError of e:string
    | ExceptionError of e:string
    | ExitCodeError of e:string
    | StartError of e:string

type StandardErrOutput = {
    Err: string
    ErrorType: ErrorType
}

type Invoke =
    Result<TimeoutInSec,string> -> ALogger.ALog -> Name -> Arguments -> Result<StandardOutput,StandardErrOutput>

module TimeoutInSec =
    let create secs =
        match secs with
        | s when s < 0 -> Error $"process timeout in seconds ({s}) must be - timeout >= 0"
        | s -> Ok (TimeoutInSec s)
module Public =

    open System.Diagnostics
    open System.Text

    let private outputHandler f (_sender: obj) (args: DataReceivedEventArgs) = f args.Data
    let private standardOutputHandler (o: StringBuilder) = outputHandler (o.Append >> ignore)

    let private addOutputReceivers o e (p: Process) =

        let addHandler (e: IEvent<DataReceivedEventHandler,DataReceivedEventArgs>, h) =
            e.AddHandler(DataReceivedEventHandler(standardOutputHandler h))

        [(p.OutputDataReceived, o); (p.ErrorDataReceived, e) ] |> List.iter addHandler
        p

    let private createProc fileName arguments =
        new Process(
            StartInfo =
                ProcessStartInfo(
                    FileName = fileName,
                    Arguments = arguments,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true
                )
        )

    let private run appendToMsgPrefix timeoutSecs (p: Process) =

        let readOutputs (p: Process) =
            [ p.BeginOutputReadLine; p.BeginErrorReadLine] |> List.iter (fun f -> f ()); p

        let defineErr errType s = s |> appendToMsgPrefix |> errType |> Error

        let waitUntil secs (p: Process) =
            match p.WaitForExit(secs * 1000) with
            | true -> (true, p.ExitCode)
            | false -> (false, p.Id)

        let checkWaitAndExitCodeOrID r =
            match r with
            | true, 0 -> Ok()
            | true, exitCode ->
                $"has unsuccessful exit code {exitCode}" |> defineErr ExitCodeError
            | false, id ->
                $"Maximum runtime ({timeoutSecs} secs) exceeded for process id [{id}]!"
                |> defineErr TimeoutError

        try
            match p.Start() with
            | true ->
                p
                |> readOutputs
                |> waitUntil timeoutSecs
                |> checkWaitAndExitCodeOrID
            | _ -> "start failure" |> defineErr StartError
        with
        | e -> $"exception - {e.Message}" |> defineErr ExceptionError

    let invoke : Invoke =
        fun timeout log name arguments ->

            let appendToMsgPrefix rest = $"Process [{name.Value}] %s{rest}"
            let dbgLog msg = appendToMsgPrefix msg |> ALogger.ALog.logPassThroughX log.Dbg

            let output = StringBuilder()
            let errors = StringBuilder()

            let eventuallyStandardErrorOutput s =
                match String.length s with
                | 0 -> "standard error output from process NOT available"
                | _ -> s

            let unwrapErrorTypeMessage err =
                match err with
                | InvalidTimeoutError e -> e
                | TimeoutError e -> e
                | ExceptionError e -> e
                | ExitCodeError e -> e
                | StartError e -> e

            let handleError e =
                log.Err (unwrapErrorTypeMessage e)
                {
                    Err = eventuallyStandardErrorOutput (errors.ToString())
                    ErrorType = e
                }

            match timeout with
            | Error err -> err |> InvalidTimeoutError |> handleError |> Error
            | Ok secs ->
                //use p = createProc name.Value arguments.Value |> addOutputReceivers output errors

                createProc name.Value arguments.Value
                |> addOutputReceivers output errors
                |> dbgLog $"with {arguments.Value}"
                |> dbgLog "is starting"
                |> run appendToMsgPrefix secs.Value
                |> dbgLog "has ended"
                |> Result.map (fun () -> output.ToString() |> StandardOutput)
                |> Result.mapError handleError