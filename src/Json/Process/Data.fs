namespace ARCtrl.Json

open Thoth.Json.Core

open ARCtrl
open ARCtrl.Process

module Data = 

    module ROCrate =

        let genID (d:Data) : string = 
            match d.ID with
            | Some id -> URI.toString id
            | None -> match d.Name with
                      | Some n -> n.Replace(" ","_")
                      | None -> "#EmptyData"
    
        let encoder (oa : Data) = 
            [
                "@id", Encode.string (oa |> genID)           
                "@type", (Encode.list [Encode.string "Data"])
                Encode.tryInclude "name" Encode.string (oa.Name)
                Encode.tryInclude "type" DataFile.ROCrate.encoder oa.DataType
                Encode.tryInclude "encodingFormat" Encode.string oa.Format
                Encode.tryInclude "usageInfo" Encode.string oa.SelectorFormat
                Encode.tryIncludeListOpt "comments" Comment.ROCrate.encoder oa.Comments
                "@context", ROCrateContext.Data.context_jsonvalue
            ]
            |> Encode.choose
            |> Encode.object

        let decoder : Decoder<Data> =
            Decode.object (fun get ->
                {
                    ID = get.Optional.Field "@id" Decode.uri
                    Name = get.Optional.Field "name" Decode.string
                    DataType = get.Optional.Field "type" DataFile.ROCrate.decoder
                    Format = get.Optional.Field "encodingFormat" Decode.string
                    SelectorFormat = get.Optional.Field "usageInfo" Decode.uri
                    Comments = get.Optional.Field "comments" (Decode.list Comment.ROCrate.decoder)
                }
            
            )


    module ISAJson =
    
        let encoder (oa : Data) = 
            [
                Encode.tryInclude "@id" Encode.string oa.ID
                Encode.tryInclude "name" Encode.string oa.Name
                Encode.tryInclude "type" DataFile.ISAJson.encoder oa.DataType
                Encode.tryIncludeListOpt "comments" Comment.ISAJson.encoder oa.Comments
            ]
            |> Encode.choose
            |> Encode.object

        let allowedFields = ["@id";"name";"type";"comments";"@type"; "@context"]

        let decoder: Decoder<Data> =
            Decode.objectNoAdditionalProperties allowedFields (fun get ->
                {
                    ID = get.Optional.Field "@id" Decode.uri
                    Name = get.Optional.Field "name" Decode.string
                    DataType = get.Optional.Field "type" DataFile.ISAJson.decoder
                    Format = None
                    SelectorFormat = None
                    Comments = get.Optional.Field "comments" (Decode.list Comment.ISAJson.decoder)
                }
            )

[<AutoOpen>]
module DataExtensions =
    
    type Data with

        static member fromISAJsonString (s:string) = 
            Decode.fromJsonString Data.ISAJson.decoder s   

        static member toISAJsonString(?spaces) =
            fun (f:Data) ->
                Data.ISAJson.encoder f
                |> Encode.toJsonString (Encode.defaultSpaces spaces)

        static member fromROCrateJsonString (s:string) =
            Decode.fromJsonString Data.ROCrate.decoder s

        static member toROCrateJsonString(?spaces) =
            fun (f:Data) ->
                Data.ROCrate.encoder f
                |> Encode.toJsonString (Encode.defaultSpaces spaces)