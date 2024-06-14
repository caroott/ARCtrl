﻿namespace ARCtrl.Json


open Thoth.Json.Core

open ARCtrl
open ARCtrl.Process


module ProtocolParameter =
    
    module ISAJson = 

        let encoder (value : ProtocolParameter) = 
            [
                Encode.tryInclude "parameterName" OntologyAnnotation.ISAJson.encoder value.ParameterName
            ]
            |> Encode.choose
            |> Encode.object

        let decoder : Decoder<ProtocolParameter> =
            Decode.object (fun get ->
                {         
                    ID = None
                    ParameterName = get.Optional.Field "parameterName" (OntologyAnnotation.ISAJson.decoder)
                }
            )

[<AutoOpen>]
module ProtocolParameterExtensions =
    
    type ProtocolParameter with
    
        static member fromISAJsonString (s:string) = 
            Decode.fromJsonString ProtocolParameter.ISAJson.decoder s   
    
        static member toISAJsonString(?spaces) =
            fun (v:ProtocolParameter) ->
                ProtocolParameter.ISAJson.encoder v
                |> Encode.toJsonString (Encode.defaultSpaces spaces)
                
        member this.ToISAJsonString(?spaces) =
            ProtocolParameter.toISAJsonString(?spaces=spaces) this