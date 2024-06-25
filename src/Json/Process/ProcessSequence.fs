﻿namespace ARCtrl.Json

open Thoth.Json.Core

open ARCtrl
open ARCtrl.Process

type ProcessSequence = 

    static member fromISAJsonString (s:string) = 
        Decode.fromJsonString (Decode.list Process.ISAJson.decoder) s  

    static member toISAJsonString(?spaces, ?useIDReferencing) =
        let useIDReferencing = Option.defaultValue false useIDReferencing
        let idMap = if useIDReferencing then Some (System.Collections.Generic.Dictionary()) else None
        fun (f:Process list) ->
            f
            |> List.map (Process.ISAJson.encoder None None idMap)
            |> Encode.list
            |> Encode.toJsonString (Encode.defaultSpaces spaces)

    static member fromROCrateJsonString (s:string) =
        Decode.fromJsonString (Decode.list Process.ROCrate.decoder) s

    static member toROCrateJsonString (?studyName : string, ?assayName : string , ?spaces) =
        fun (f:Process list) ->
            f
            |> List.map (Process.ROCrate.encoder studyName assayName)
            |> Encode.list
            |> Encode.toJsonString (Encode.defaultSpaces spaces)