namespace ARCtrl.Spreadsheet

open ARCtrl
open FsSpreadsheet

open ARCtrl.Helper

module ArcStudy = 

    let [<Literal>] obsoleteStudiesLabel = "STUDY METADATA"
    let [<Literal>] studiesLabel = "STUDY"

    let [<Literal>] obsoleteMetaDataSheetName = "Study"
    let [<Literal>] metaDataSheetName = "isa_study"

    let toMetadataSheet (study : ArcStudy) (assays : ArcAssay list option) : FsWorksheet =
        //let toRows (study:ArcStudy) assays =
        //    seq {          
        //        yield  SparseRow.fromValues [studiesLabel]
        //        yield! Studies.StudyInfo.toRows study
        //    }
        let sheet = FsWorksheet(metaDataSheetName)
        Studies.toRows study assays
        |> Seq.append [SparseRow.fromValues [studiesLabel]]
        |> Seq.iteri (fun rowI r -> SparseRow.writeToSheet (rowI + 1) r sheet)    
        sheet

    let fromMetadataSheet (sheet : FsWorksheet) : ArcStudy*ArcAssay list =
        try
            let fromRows (rows: seq<SparseRow>) =
                let en = rows.GetEnumerator()
                en.MoveNext() |> ignore  
                let _,_,_,study = Studies.fromRows 2 en
                study
            sheet.Rows 
            |> Seq.map SparseRow.fromFsRow
            |> fromRows
            |> Option.defaultValue (ArcStudy.create(Identifier.createMissingIdentifier()),[])
        with 
        | err -> failwithf "Failed while parsing metadatasheet: %s" err.Message

[<AutoOpen>]
module ArcStudyExtensions =

    type ArcStudy with
    
        /// Reads an assay from a spreadsheet
        static member fromFsWorkbook (doc:FsWorkbook) = 
            try
                // Reading the "Assay" metadata sheet. Here metadata 
                let studyMetadata,assays = 
        
                    match doc.TryGetWorksheetByName ArcStudy.metaDataSheetName with 
                    | Option.Some sheet ->
                        ArcStudy.fromMetadataSheet sheet
                    | None ->  
                        match doc.TryGetWorksheetByName ArcStudy.obsoleteMetaDataSheetName with 
                        | Option.Some sheet ->
                            ArcStudy.fromMetadataSheet sheet
                        | None -> 
                            printfn "Cannot retrieve metadata: Study file does not contain \"%s\" or \"%s\" sheet." ArcStudy.metaDataSheetName ArcStudy.obsoleteMetaDataSheetName
                            ArcStudy.create(Identifier.createMissingIdentifier()),[]
                let sheets = doc.GetWorksheets()
                let annotationTables = 
                    sheets
                    |> ResizeArray.choose ArcTable.tryFromFsWorksheet
                // Performance hotfix. This change is tested in ISA.Spreadsheet/Performance.Tests.fs and results in 2 pendings tests in ARCtrl/ARCtrl.Tests.fs.
                //if annotationTables |> Seq.isEmpty |> not then 
                //    let updatedTables = 
                //            ArcTables.updateReferenceTablesBySheets( // This only kills performance with ProtocolREF
                //                (ArcTables studyMetadata.Tables),
                //                (ArcTables (ResizeArray annotationTables)),
                //                keepUnusedRefTables =  true
                //                )
                //    studyMetadata.Tables <- updatedTables.Tables
                let datamapSheet =
                    sheets |> Seq.tryPick DataMapTable.tryFromFsWorksheet

                if annotationTables |> ResizeArray.isEmpty |> not then
                    studyMetadata.Tables <- annotationTables
                studyMetadata.DataMap <- datamapSheet

                studyMetadata,assays
            with
            | err -> failwithf "Could not parse study: \n%s" err.Message

        /// <summary>
        /// Write a study to a spreadsheet
        ///
        /// If datamapSheet is true, the datamap will be written to a worksheet inside study workbook. Default: true
        /// </summary>
        /// <param name="study"></param>
        /// <param name="assays"></param>
        /// <param name="datamapSheet"></param>
        static member toFsWorkbook (study : ArcStudy, ?assays : ArcAssay list, ?datamapSheet: bool) =
            let datamapSheet = defaultArg datamapSheet true
            let doc = new FsWorkbook()
            let metaDataSheet = ArcStudy.toMetadataSheet study assays
            doc.AddWorksheet metaDataSheet

            if datamapSheet then
                study.DataMap
                |> Option.iter (DataMapTable.toFsWorksheet >> doc.AddWorksheet)

            study.Tables
            |> ResizeArray.iter (ArcTable.toFsWorksheet >> doc.AddWorksheet)

            doc