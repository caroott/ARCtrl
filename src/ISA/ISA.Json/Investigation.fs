namespace ARCtrl.ISA.Json

open Thoth.Json.Core

open ARCtrl.ISA

    // let fromJsonString (s:string) = 
    //     GDecode.fromJsonString (decoder (ConverterOptions())) s

    // let toJsonString (p:Investigation) = 
    //     encoder (ConverterOptions()) p
    //     |> GEncode.toJsonString 2

    // /// exports in json-ld format
    // let toJsonldString (i:Investigation) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true)) i
    //     |> GEncode.toJsonString 2

    // let toJsonldStringWithContext (i:Investigation) = 
    //     encoder (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true)) i
    //     |> GEncode.toJsonString 2

    // let toRoCrateString (i:Investigation) = 
    //     encodeRoCrate (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true,IsRoCrate=true)) i
    //     |> GEncode.toJsonString 2