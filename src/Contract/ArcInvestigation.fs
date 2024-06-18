namespace ARCtrl.Contract

open ARCtrl.FileSystem
open ARCtrl.Path
open ARCtrl.Spreadsheet
open ARCtrl
open ARCtrl.Helper
open FsSpreadsheet


[<AutoOpen>]
module InvestigationContractExtensions = 

    let (|InvestigationPath|_|) (input) =
        match input with
        | [|InvestigationFileName|] -> 
            let path = ARCtrl.Path.combineMany input
            Some path
        | _ -> None

    module ArcInvestigation =

        //let ToDeleteContract () =
        //    let path = InvestigationFileName
        //    let c = Contract.createDelete(path)
        //    c
        
        //static member toDeleteContract () : Contract =
        //    let path = InvestigationFileName
        //    let c = Contract.createDelete(path)
        //    c

        let toCreateContract (inv: ArcInvestigation, isLight: bool option) : Contract =
            let isLight = defaultArg isLight true
            let converter = if isLight then ArcInvestigation.toLightFsWorkbook else ArcInvestigation.toFsWorkbook
            let path = InvestigationFileName
            let c = Contract.createCreate(path, DTOType.ISA_Investigation, DTO.Spreadsheet (inv |> converter))
            c

        let toUpdateContract (inv: ArcInvestigation, isLight: bool option) : Contract =
            let isLight = defaultArg isLight true
            let converter = if isLight then ArcInvestigation.toLightFsWorkbook else ArcInvestigation.toFsWorkbook
            let path = InvestigationFileName
            let c = Contract.createUpdate(path, DTOType.ISA_Investigation, DTO.Spreadsheet (inv |> converter))
            c

        let tryFromReadContract (c:Contract) =
            match c with
            | {Operation = READ; DTOType = Some DTOType.ISA_Investigation; DTO = Some (DTO.Spreadsheet fsworkbook)} ->
                fsworkbook :?> FsWorkbook
                |> ArcInvestigation.fromFsWorkbook
                |> Some 
            | _ -> None
      