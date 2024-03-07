namespace ARCtrl.ISA.Json

open Thoth.Json.Core

open ARCtrl.ISA
open System.IO

    // let fromJsonString (s:string) = 
    //     GDecode.fromJsonString (decoder (ConverterOptions())) s

    // let toJsonString (p:Assay) = 
    //     encoder (ConverterOptions()) None p
    //     |> GEncode.toJsonString 2

    /// exports in json-ld format
    // let toJsonldString (a:Assay) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true)) None a
    //     |> GEncode.toJsonString 2

    // let toJsonldStringWithContext (a:Assay) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true)) None a
    //     |> GEncode.toJsonString 2

    //let fromFile (path : string) = 
    //    File.ReadAllText path 
    //    |> fromString

    //let toFile (path : string) (p:Assay) = 
    //    File.WriteAllText(path,toString p)