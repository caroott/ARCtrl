namespace ARCtrl.ISA.Json

open Thoth.Json.Core

open ARCtrl.ISA   

open JsonHelper

module Investigation =
    
    
    let genID (i:ArcInvestigation) : string = 
        "./"
        // match i.ID with
        // | Some id -> URI.toString id
        // | None -> match i.FileName with
        //           | Some n -> "#Study_" + n.Replace(" ","_")
        //           | None -> match i.Identifier with
        //                     | Some id -> "#Study_" + id.Replace(" ","_")
        //                     | None -> match i.Title with
        //                               | Some t -> "#Study_" + t.Replace(" ","_")
        //                               | None -> "#EmptyStudy"

    let encoder (options : ConverterOptions) (oa : ArcInvestigation) = 
        let identifier =
            if ARCtrl.ISA.Identifier.isMissingIdentifier oa.Identifier then None
            else Some oa.Identifier
        [
            "@id", Encode.string (oa |> genID)
            if options.IncludeType then 
                "@type", Encode.string "Investigation"
            "filename", Encode.string ARCtrl.Path.InvestigationFileName
            GEncode.tryInclude "identifier" Encode.string (identifier)
            GEncode.tryInclude "title" Encode.string (oa.Title)
            GEncode.tryInclude "description" Encode.string (oa.Description)
            GEncode.tryInclude "submissionDate" Encode.string (oa.SubmissionDate)
            GEncode.tryInclude "publicReleaseDate" Encode.string (oa.PublicReleaseDate)
            GEncode.tryIncludeList "ontologySourceReferences" (OntologySourceReference.encoder options) (oa.OntologySourceReferences |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "publications" (Publication.encoder options) (oa.Publications |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "people" (Person.encoder options) (oa.Contacts |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "studies" (Study.encoder options) (oa.RegisteredStudies |> Seq.toList |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "comments" (Comment.encoder options) (oa.Comments |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            if options.IncludeContext then
                "@context", ROCrateContext.Investigation.context_jsonvalue
        ]
        |> GEncode.choose
        |> Encode.object

    let encodeRoCrate (options : ConverterOptions) (oa : ArcInvestigation) = 
        [
            GEncode.tryInclude "@type" Encode.string (Some "CreativeWork")
            GEncode.tryInclude "@id" Encode.string (Some "ro-crate-metadata.json")
            GEncode.tryInclude "about" (encoder options) (Some oa)
            "conformsTo", ROCrateContext.ROCrate.conformsTo_jsonvalue
            if options.IncludeContext then
                "@context", ROCrateContext.ROCrate.context_jsonvalue
            ]
        |> GEncode.choose
        |> Encode.object


    let allowedFields = ["@id";"filename";"identifier";"title";"description";"submissionDate";"publicReleaseDate";"ontologySourceReferences";"publications";"people";"studies";"comments";"@type";"@context"]

    let decoder (options : ConverterOptions) : Decoder<ArcInvestigation> =
        // let DecodeAssays : Decoder<ArcAssay list> = Decode.list ArcAssay.decoder 
        // let DecodeStudies : Decoder<ArcStudy list> = Decode.list ArcStudy.decoder
        // let tryGetAssays (get:Decode.IGetters) (fieldName:string) = get.Optional.Field(fieldName) DecodeAssays |> Option.map ResizeArray |> Option.defaultValue (ResizeArray())
        // let tryGetStudies (get:Decode.IGetters) (fieldName:string) = get.Optional.Field(fieldName) DecodeStudies |> Option.map ResizeArray |> Option.defaultValue (ResizeArray()) 
        Decode.object (fun get ->
        ArcInvestigation.make 
            (get.Required.Field("Identifier") Decode.string)
            (get.Optional.Field("Title") Decode.string)
            (get.Optional.Field("Description") Decode.string)
            (get.Optional.Field("SubmissionDate") Decode.string)
            (get.Optional.Field("PublicReleaseDate") Decode.string)
            [||]//(tryGetOntologySourceReferences get "OntologySourceReferences")
            [||]//(tryGetPublications get "Publications")
            [||]//(tryGetPersons get "Contacts")
            (ResizeArray<_> [])//(tryGetAssays get "Assays")
            (ResizeArray<_> [])//(tryGetStudies get "Studies")
            (ResizeArray<_> [])//(tryGetStringResizeArray get "RegisteredStudyIdentifiers")
            [||]//(tryGetComments get "Comments")
            [||]
        )
        // GDecode.object allowedFields (fun get ->
        //     {
        //         Identifier = get.Optional.Field "identifier" Decode.string
        //         Title = get.Optional.Field "title" Decode.string
        //         Description = get.Optional.Field "description" Decode.string
        //         SubmissionDate = get.Optional.Field "submissionDate" Decode.string
        //         PublicReleaseDate = get.Optional.Field "publicReleaseDate" Decode.string
        //         OntologySourceReferences = get.Optional.Field "ontologySourceReferences" (Decode.list (OntologySourceReference.decoder options))
        //         Publications = get.Optional.Field "publications" (Decode.list (Publication.decoder options))
        //         Contacts = get.Optional.Field "people" (Decode.list (Person.decoder options))
        //         Studies = get.Optional.Field "studies" (Decode.list (Study.decoder options))
        //         Comments = get.Optional.Field "comments" (Decode.list (Comment.decoder options))
        //         Remarks = []
        //     }
        // )

module ArcInvestigation = 
    let encoder (inv:ArcInvestigation) = 
        Encode.object [ 
            "Identifier", Encode.string inv.Identifier
            if inv.Title.IsSome then
                "Title", Encode.string inv.Title.Value
            if inv.Description.IsSome then
                "Description", Encode.string inv.Description.Value
            if inv.SubmissionDate.IsSome then
                "SubmissionDate", Encode.string inv.SubmissionDate.Value
            if inv.PublicReleaseDate.IsSome then
                "PublicReleaseDate", Encode.string inv.PublicReleaseDate.Value
            if inv.OntologySourceReferences.Length <> 0 then
                "OntologySourceReferences", EncoderOntologySourceReferences inv.OntologySourceReferences
            if inv.Publications.Length <> 0 then
                "Publications", EncoderPublications inv.Publications
            if inv.Contacts.Length <> 0 then
                "Contacts", EncoderPersons inv.Contacts
            if inv.Assays.Count <> 0 then
                "Assays", Encode.seq (Seq.map ArcAssay.encoder inv.Assays) 
            if inv.Studies.Count <> 0 then
                "Studies", Encode.seq (Seq.map ArcStudy.encoder inv.Studies)
            if inv.RegisteredStudyIdentifiers.Count <> 0 then
                "RegisteredStudyIdentifiers", Encode.seq (Seq.map Encode.string inv.RegisteredStudyIdentifiers)
            if inv.Comments.Length <> 0 then
                "Comments", EncoderComments inv.Comments
            // remarks are ignored for whatever reason
        ]
  
    let decoder : Decoder<ArcInvestigation> =
        let DecodeAssays : Decoder<ArcAssay list> = Decode.list ArcAssay.decoder 
        let DecodeStudies : Decoder<ArcStudy list> = Decode.list ArcStudy.decoder
        let tryGetAssays (get:Decode.IGetters) (fieldName:string) = get.Optional.Field(fieldName) DecodeAssays |> Option.map ResizeArray |> Option.defaultValue (ResizeArray())
        let tryGetStudies (get:Decode.IGetters) (fieldName:string) = get.Optional.Field(fieldName) DecodeStudies |> Option.map ResizeArray |> Option.defaultValue (ResizeArray()) 
        Decode.object (fun get ->
        ArcInvestigation.make 
            (get.Required.Field("Identifier") Decode.string)
            (get.Optional.Field("Title") Decode.string)
            (get.Optional.Field("Description") Decode.string)
            (get.Optional.Field("SubmissionDate") Decode.string)
            (get.Optional.Field("PublicReleaseDate") Decode.string)
            (tryGetOntologySourceReferences get "OntologySourceReferences")
            (tryGetPublications get "Publications")
            (tryGetPersons get "Contacts")
            (tryGetAssays get "Assays")
            (tryGetStudies get "Studies")
            (tryGetStringResizeArray get "RegisteredStudyIdentifiers")
            (tryGetComments get "Comments")
            [||]
        )

    /// exports in json-ld format
    let toJsonldString (a:ArcInvestigation) = 
        Investigation.encoder (ConverterOptions(SetID=true,IncludeType=true)) a
        |> GEncode.toJsonString 2

    let toJsonldStringWithContext (a:ArcInvestigation) = 
        Investigation.encoder (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true)) a
        |> GEncode.toJsonString 2

    let toRoCrateString (i:ArcInvestigation) = 
        Investigation.encodeRoCrate (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true,IsRoCrate=true)) i
        |> GEncode.toJsonString 2

    let fromJsonString (s:string) = 
        GDecode.fromJsonString (Investigation.decoder (ConverterOptions())) s
        // |> ArcInvestigation.fromInvestigation

    let toJsonString (a:ArcInvestigation) = 
        Investigation.encoder (ConverterOptions()) a
        |> GEncode.toJsonString 2

    let toArcJsonString (a:ArcInvestigation) : string =
        let spaces = 0
        GEncode.toJsonString spaces (encoder a)

    let fromArcJsonString (jsonString: string) =
        try GDecode.fromJsonString decoder jsonString with
        | e -> failwithf "Error. Unable to parse json string to ArcInvestigation: %s" e.Message

[<AutoOpen>]
module ArcInvestigationExtensions =

    type ArcInvestigation with
        static member fromArcJsonString (jsonString: string) : ArcInvestigation = 
            try GDecode.fromJsonString ArcInvestigation.decoder jsonString with
            | e -> failwithf "Error. Unable to parse json string to ArcInvestigation: %s" e.Message

        member this.ToArcJsonString(?spaces) : string =
            let spaces = defaultArg spaces 0
            GEncode.toJsonString spaces (ArcInvestigation.encoder this)

        static member toArcJsonString(a:ArcInvestigation) = a.ToArcJsonString()