﻿module ArcStudyTests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open ARCtrl.ISA
open TestingUtils
open ARCtrl.ISA.Spreadsheet

let testMetaDataFunctions = 

    testList "StudyMetadataTests" [
        testCase "ReaderSuccessEmpty" (fun () -> 
            
            let readingSuccess = 
                try 
                    ArcStudy.fromMetadataSheet TestObjects.Study.studyMetadataEmpty |> ignore
                    Result.Ok "DidRun"
                with
                | err -> Result.Error(sprintf "Reading the empty test file failed: %s" err.Message)
            Expect.isOk readingSuccess (Result.getMessage readingSuccess)
        )

        testCase "ReaderSuccessEmptyObsoleteSheetName" (fun () -> 
            
            let readingSuccess = 
                try 
                    ArcStudy.fromMetadataSheet TestObjects.Study.studyMetadataEmptyObsoleteSheetName |> ignore
                    Result.Ok "DidRun"
                with
                | err -> Result.Error(sprintf "Reading the empty test file failed: %s" err.Message)
            Expect.isOk readingSuccess (Result.getMessage readingSuccess)
        )

        testCase "WriterSuccessEmpty" (fun () ->

            let a = ArcStudy.fromMetadataSheet TestObjects.Study.studyMetadataEmpty

            let writingSuccess = 
                try 
                    ArcStudy.toMetadataSheet a |> ignore
                    Result.Ok "DidRun"
                with
                | err -> Result.Error(sprintf "Writing the Empty test file failed: %s" err.Message)

            Expect.isOk writingSuccess (Result.getMessage writingSuccess)
        )

        testCase "WriterSuccessEmptyObsoleteSheetName" (fun () ->

            let a = 
                ArcStudy.fromMetadataSheet TestObjects.Study.studyMetadataEmptyObsoleteSheetName
            let writingSuccess = 
                try 
                    ArcStudy.toMetadataSheet a |> ignore
                    Result.Ok "DidRun"
                with
                | err -> Result.Error(sprintf "Writing the Empty test file failed: %s" err.Message)

            Expect.isOk writingSuccess (Result.getMessage writingSuccess)
        )

        testCase "OutputMatchesInputEmpty" (fun () ->
           
            let o = 
                TestObjects.Study.studyMetadataEmpty
                |> ArcStudy.fromMetadataSheet
                |> ArcStudy.toMetadataSheet

            Expect.workSheetEqual o TestObjects.Study.studyMetadataEmpty "Written Empty study metadata does not match read study metadata"
        )

        testCase "OutputSheetNamesDifferentEmptyObsoleteSheetName" (fun () ->
           
            let o = 
                TestObjects.Study.studyMetadataEmptyObsoleteSheetName
                |> ArcStudy.fromMetadataSheet
                |> ArcStudy.toMetadataSheet

            Expect.isTrue (o.Name <> TestObjects.Study.studyMetadataEmptyObsoleteSheetName.Name) "sheet names were expected to be different (obsolete replaced by new)"
        )
    ]

let main = 
    testList "StudyFile" [
        testMetaDataFunctions
    ]