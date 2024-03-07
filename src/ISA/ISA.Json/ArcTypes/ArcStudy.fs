namespace ARCtrl.ISA.Json

open Thoth.Json.Core

open ARCtrl.ISA

open JsonHelper

module StudyMaterials = 

    let encoder (options : ConverterOptions) (oa : StudyMaterials) = 
        [
            GEncode.tryIncludeList "sources" (Source.encoder options) (oa.Sources)
            GEncode.tryIncludeList "samples" (Sample.encoder options) (oa.Samples)
            GEncode.tryIncludeList "otherMaterials" (Material.encoder options) (oa.OtherMaterials)
        ]
        |> GEncode.choose
        |> Encode.object
    
    let allowedFields = ["sources";"samples";"otherMaterials"]

    let decoder (options : ConverterOptions) : Decoder<StudyMaterials> =
        GDecode.object allowedFields (fun get ->
            {
                Sources = get.Optional.Field "sources" (Decode.list (Source.decoder options))
                Samples = get.Optional.Field "samples" (Decode.list (Sample.decoder options))
                OtherMaterials = get.Optional.Field "otherMaterials" (Decode.list (Material.decoder options))
            }
        )


module Study =
    
    let genID (s:ArcStudy) : string =
        let fileName = 
            if ARCtrl.ISA.Identifier.isMissingIdentifier s.Identifier then
                None
            else
                Some (Identifier.Study.fileNameFromIdentifier s.Identifier) 
        match fileName with
        | Some n -> n.Replace(" ","_").Remove(0,1 + (max (n.LastIndexOf('/')) (n.LastIndexOf('\\'))))
        | None -> match s.Title with
                    | Some t -> "#Study_" + t.Replace(" ","_")
                    | None -> "#EmptyStudy"
    
    let encoder (options : ConverterOptions) (oa : ArcStudy) = 
        let identifier,fileName = 
            if ARCtrl.ISA.Identifier.isMissingIdentifier oa.Identifier then
                None, None
            else
                Some oa.Identifier, Some (Identifier.Study.fileNameFromIdentifier oa.Identifier)
        let assays = 
            List.ofSeq (oa.GetRegisteredAssaysOrIdentifier())
        let processSeq = ArcTables(oa.Tables).GetProcesses()
        let protocols = ProcessSequence.getProtocols processSeq
        let studySources = ProcessSequence.getSources processSeq
        let studySamples = ProcessSequence.getSamples processSeq
        let studyMaterials = ProcessSequence.getMaterials processSeq
        [
            "@id", Encode.string (oa |> genID)
            if options.IncludeType then 
                "@type", (Encode.list [Encode.string "Study"; Encode.string "ArcStudy"])
            GEncode.tryInclude "filename" Encode.string (fileName)
            GEncode.tryInclude "identifier" Encode.string (identifier)
            GEncode.tryInclude "title" Encode.string (oa.Title)
            GEncode.tryInclude "description" Encode.string (oa.Description)
            GEncode.tryInclude "submissionDate" Encode.string (oa.SubmissionDate)
            GEncode.tryInclude "publicReleaseDate" Encode.string (oa.PublicReleaseDate)
            "publications", (oa.Publications |> Array.map (Publication.encoder options) |> Encode.array)  
            "people", (oa.Contacts |> Array.map (Person.encoder options) |> Encode.array)
            "studyDesignDescriptors", (oa.StudyDesignDescriptors |> Array.map (OntologyAnnotation.encoder options) |> Encode.array)
            if not options.IsRoCrate then 
                GEncode.tryIncludeList "protocols" (Protocol.encoder options None None None) (protocols |> Aux.Option.fromValueWithDefault [])
            if options.IsRoCrate then
                "samples", (studySamples |> List.map (Sample.encoder options) |> Encode.list)
                "sources", (studySources |> List.map (Source.encoder options) |> Encode.list)
                "materials", (studyMaterials |> List.map (Material.encoder options) |> Encode.list)
            if not options.IsRoCrate then 
                let studyMaterials =
                    StudyMaterials.create(
                        ?Sources = (studySources |> Aux.Option.fromValueWithDefault []),
                        ?Samples = (studySamples |> Aux.Option.fromValueWithDefault []),
                        ?OtherMaterials = (studyMaterials |> Aux.Option.fromValueWithDefault []))
                    |> Aux.Option.fromValueWithDefault StudyMaterials.empty
                GEncode.tryInclude "materials" (StudyMaterials.encoder options) studyMaterials
            GEncode.tryIncludeList "processSequence" (Process.encoder options identifier None) (processSeq |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "assays" (Assay.encoder options identifier) (assays |> Aux.Option.fromValueWithDefault [])            
            GEncode.tryIncludeList "factors" (Factor.encoder options) (oa.Factors |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "characteristicCategories" (MaterialAttribute.encoder options) (ProcessSequence.getCharacteristics processSeq |> Aux.Option.fromValueWithDefault [])            
            GEncode.tryIncludeList "unitCategories" (OntologyAnnotation.encoder options) (ProcessSequence.getUnits processSeq |> Aux.Option.fromValueWithDefault [])
            GEncode.tryIncludeList "comments" (Comment.encoder options) (oa.Comments |> List.ofArray |> Aux.Option.fromValueWithDefault [])
            if options.IncludeContext then 
                "@context", ROCrateContext.Study.context_jsonvalue
        ]
        |> GEncode.choose
        |> Encode.object

    let allowedFields = ["@id";"filename";"identifier";"title";"description";"submissionDate";"publicReleaseDate";"publications";"people";"studyDesignDescriptors";"protocols";"materials";"assays";"factors";"characteristicCategories";"unitCategories";"processSequence";"comments";"@type"; "@context"]

    let decoder (options : ConverterOptions) : Decoder<ArcStudy> =
        Decode.object (fun get ->
            ArcStudy.make
                (get.Required.Field("identifier") Decode.string)
                (get.Optional.Field("title") Decode.string)
                (get.Optional.Field("description") Decode.string)
                (get.Optional.Field("submissionDate") Decode.string)
                (get.Optional.Field("publicReleaseDate") Decode.string)
                [||]//(get.Optional.Field "publications" (Decode.list (Publication.decoder options)))
                [||]//(get.Optional.Field "people" (Decode.list (Person.decoder options)))
                [||]//(get.Optional.Field "studyDesignDescriptors" (Decode.list (OntologyAnnotation.decoder options)))
                (ResizeArray<_> [])//(tryGetTables get "Tables")
                (ResizeArray<_> [])//(get.Optional.Field "assays" (Decode.list (Assay.decoder options)))
                [||]//(get.Optional.Field "factors" (Decode.list (Factor.decoder options)))
                [||]//(get.Optional.Field "comments" (Decode.list (Comment.decoder options)))
                
                // Protocols = get.Optional.Field "protocols" (Decode.list (Protocol.decoder options))
                // Materials = get.Optional.Field "materials" (StudyMaterials.decoder options)
                // CharacteristicCategories = get.Optional.Field "characteristicCategories" (Decode.list (MaterialAttribute.decoder options))
                // UnitCategories = get.Optional.Field "unitCategories" (Decode.list (OntologyAnnotation.decoder options))
                // ProcessSequence = get.Optional.Field "processSequence" (Decode.list (Process.decoder options))
        )

module ArcStudy = 
    let encoder (study:ArcStudy) = 
        Encode.object [ 
            "Identifier", Encode.string study.Identifier
            if study.Title.IsSome then
                "Title", Encode.string study.Title.Value
            if study.Description.IsSome then
                "Description", Encode.string study.Description.Value
            if study.SubmissionDate.IsSome then
                "SubmissionDate", Encode.string study.SubmissionDate.Value
            if study.PublicReleaseDate.IsSome then
                "PublicReleaseDate", Encode.string study.PublicReleaseDate.Value
            if study.Publications.Length <> 0 then
                "Publications", EncoderPublications study.Publications
            if study.Contacts.Length <> 0 then
                "Contacts", EncoderPersons study.Contacts
            if study.StudyDesignDescriptors.Length <> 0 then
                "StudyDesignDescriptors", EncoderOAs study.StudyDesignDescriptors
            if study.TableCount <> 0 then
                "Tables", EncoderTables study.Tables
            if study.RegisteredAssayIdentifiers.Count <> 0 then
                "RegisteredAssayIdentifiers", Encode.seq (Seq.map Encode.string study.RegisteredAssayIdentifiers)
            if study.Factors.Length <> 0 then
                "Factors", EncoderFactors study.Factors
            if study.Comments.Length <> 0 then
                "Comments", EncoderComments study.Comments
        ]
  
    let decoder : Decoder<ArcStudy> =
        Decode.object (fun get ->
            ArcStudy.make 
                (get.Required.Field("Identifier") Decode.string)
                (get.Optional.Field("Title") Decode.string)
                (get.Optional.Field("Description") Decode.string)
                (get.Optional.Field("SubmissionDate") Decode.string)
                (get.Optional.Field("PublicReleaseDate") Decode.string)
                (tryGetPublications get "Publications")
                (tryGetPersons get "Contacts")
                (tryGetOAs get "StudyDesignDescriptors")
                (tryGetTables get "Tables")
                (tryGetStringResizeArray get "RegisteredAssayIdentifiers")
                (tryGetFactors get "Factors")
                (tryGetComments get "Comments")
    )

    let compressedEncoder (stringTable : StringTableMap) (oaTable : OATableMap) (cellTable : CellTableMap) (study:ArcStudy) =
        Encode.object [ 
            "Identifier", Encode.string study.Identifier
            if study.Title.IsSome then
                "Title", Encode.string study.Title.Value
            if study.Description.IsSome then
                "Description", Encode.string study.Description.Value
            if study.SubmissionDate.IsSome then
                "SubmissionDate", Encode.string study.SubmissionDate.Value
            if study.PublicReleaseDate.IsSome then
                "PublicReleaseDate", Encode.string study.PublicReleaseDate.Value
            if study.Publications.Length <> 0 then
                "Publications", EncoderPublications study.Publications
            if study.Contacts.Length <> 0 then
                "Contacts", EncoderPersons study.Contacts
            if study.StudyDesignDescriptors.Length <> 0 then
                "StudyDesignDescriptors", EncoderOAs study.StudyDesignDescriptors
            if study.TableCount <> 0 then
                "Tables", Encode.seq (Seq.map (ArcTable.compressedEncoder stringTable oaTable cellTable) study.Tables) 
            if study.RegisteredAssayIdentifiers.Count <> 0 then
                "RegisteredAssayIdentifiers", Encode.seq (Seq.map Encode.string study.RegisteredAssayIdentifiers)
            if study.Factors.Length <> 0 then
                "Factors", EncoderFactors study.Factors
            if study.Comments.Length <> 0 then
                "Comments", EncoderComments study.Comments
        ]

    let compressedDecoder (stringTable : StringTableArray) (oaTable : OATableArray) (cellTable : CellTableArray) : Decoder<ArcStudy> =
        Decode.object (fun get ->
            let tables = 
                get.Optional.Field("Tables") (Decode.array (ArcTable.compressedDecoder stringTable oaTable cellTable))
                |> Option.map ResizeArray 
                |> Option.defaultValue (ResizeArray())
            ArcStudy.make 
                (get.Required.Field("Identifier") Decode.string)
                (get.Optional.Field("Title") Decode.string)
                (get.Optional.Field("Description") Decode.string)
                (get.Optional.Field("SubmissionDate") Decode.string)
                (get.Optional.Field("PublicReleaseDate") Decode.string)
                (tryGetPublications get "Publications")
                (tryGetPersons get "Contacts")
                (tryGetOAs get "StudyDesignDescriptors")
                tables
                (tryGetStringResizeArray get "RegisteredAssayIdentifiers")
                (tryGetFactors get "Factors")
                (tryGetComments get "Comments")
        )


    /// exports in json-ld format
    let toJsonldString (a:ArcStudy) (assays: ResizeArray<ArcAssay>) = 
        Study.encoder (ConverterOptions(SetID=true,IncludeType=true)) a
        |> GEncode.toJsonString 2

    let toJsonldStringWithContext (a:ArcStudy) (assays: ResizeArray<ArcAssay>) = 
        Study.encoder (ConverterOptions(SetID=true,IncludeType=true,IncludeContext=true)) a
        |> GEncode.toJsonString 2

    let fromJsonString (s:string) = 
        GDecode.fromJsonString (Study.decoder (ConverterOptions())) s
        // |> ArcStudy.fromStudy

    let toJsonString (a:ArcStudy) (assays: ResizeArray<ArcAssay>) = 
        Study.encoder (ConverterOptions()) a
        |> GEncode.toJsonString 2

    let toArcJsonString (a:ArcStudy) : string =
        let spaces = 0
        GEncode.toJsonString spaces (encoder a)

    let fromArcJsonString (jsonString: string) =
        try GDecode.fromJsonString decoder jsonString with
        | e -> failwithf "Error. Unable to parse json string to ArcStudy: %s" e.Message

[<AutoOpen>]
module ArcStudyExtensions =
    
    open System.Collections.Generic

    type ArcStudy with
        static member fromArcJsonString (jsonString: string) : ArcStudy = 
            try GDecode.fromJsonString ArcStudy.decoder jsonString with
            | e -> failwithf "Error. Unable to parse json string to ArcStudy: %s" e.Message

        member this.ToArcJsonString(?spaces) : string =
            let spaces = defaultArg spaces 0
            GEncode.toJsonString spaces (ArcStudy.encoder this)

        static member toArcJsonString(a:ArcStudy) = a.ToArcJsonString()

        static member fromCompressedJsonString (jsonString: string) : ArcStudy = 
            let decoder = 
                Decode.object(fun get ->
                    let stringTable = get.Required.Field "stringTable" (StringTable.decoder)
                    let oaTable = get.Required.Field "oaTable" (OATable.decoder stringTable)
                    let cellTable = get.Required.Field "cellTable" (CellTable.decoder stringTable oaTable)
                    get.Required.Field "study" (ArcStudy.compressedDecoder stringTable oaTable cellTable)
                )
            try GDecode.fromJsonString decoder jsonString with
            | e -> failwithf "Error. Unable to parse json string to ArcAssay: %s" e.Message

        member this.ToCompressedJsonString(?spaces) : string =
            let spaces = defaultArg spaces 0
            let stringTable = Dictionary()
            let oaTable = Dictionary()
            let cellTable = Dictionary()
            let arcStudy = ArcStudy.compressedEncoder stringTable oaTable cellTable this
            let jObject = 
                Encode.object [
                    "cellTable", CellTable.arrayFromMap cellTable |> CellTable.encoder stringTable oaTable
                    "oaTable", OATable.arrayFromMap oaTable |> OATable.encoder stringTable
                    "stringTable", StringTable.arrayFromMap stringTable |> StringTable.encoder
                    "study", arcStudy
                ] 
            GEncode.toJsonString spaces jObject

        static member toCompressedJsonString (s : ArcStudy) = 
            s.ToCompressedJsonString()