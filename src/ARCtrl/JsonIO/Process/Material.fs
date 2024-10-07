namespace ARCtrl.Json

open ARCtrl.Process

[<AutoOpen>]
module MaterialExtensions =
    
    type Material with

        static member fromISAJsonString (s:string) = 
            Decode.fromJsonString Material.ISAJson.decoder s   

        static member toISAJsonString(?spaces, ?useIDReferencing) =
            let useIDReferencing = Option.defaultValue false useIDReferencing
            let idMap = if useIDReferencing then Some (System.Collections.Generic.Dictionary()) else None
            fun (f:Material) ->
                Material.ISAJson.encoder idMap f
                |> Encode.toJsonString (Encode.defaultSpaces spaces)

        member this.ToISAJsonString(?spaces, ?useIDReferencing) =
            Material.toISAJsonString(?spaces=spaces, ?useIDReferencing = useIDReferencing) this

        static member fromROCrateJsonString (s:string) =
            Decode.fromJsonString Material.ROCrate.decoder s

        static member toROCrateJsonString(?spaces) =
            fun (f:Material) ->
                Material.ROCrate.encoder f
                |> Encode.toJsonString (Encode.defaultSpaces spaces)

        member this.ToROCrateJsonString(?spaces) =
            Material.toROCrateJsonString(?spaces=spaces) this