namespace ARCtrl.ISA.Json

open Thoth.Json.Core

open ARCtrl.ISA
open System.IO

    // let fromJsonString (s:string) = 
    //     GDecode.fromJsonString (decoder (ConverterOptions())) s

    // let toJsonString (p:Study) = 
    //     encoder (ConverterOptions()) p
    //     |> GEncode.toJsonString 2

    // /// exports in json-ld format
    // let toJsonldString (s:Study) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true)) s
    //     |> GEncode.toJsonString 2

    // let toJsonldStringWithContext (a:Study) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true)) a
    //     |> GEncode.toJsonString 2