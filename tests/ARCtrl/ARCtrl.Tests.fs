module ARCtrl.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open ARCtrl
open ARCtrl.ISA

let private test_model = testList "model" [
    testCase "create" <| fun _ ->
        let arc = ARC()
        Expect.isNone arc.CWL "cwl"
        Expect.isNone arc.ISA "isa"
    testCase "fromFilePath" <| fun _ ->
        let input = 
            [|@"isa.investigation.xlsx"; @".arc\.gitkeep"; @".git\config";
            @".git\description"; @".git\HEAD"; @"assays\.gitkeep"; @"runs\.gitkeep";
            @"studies\.gitkeep"; @"workflows\.gitkeep";
            @".git\hooks\applypatch-msg.sample"; @".git\hooks\commit-msg.sample";
            @".git\hooks\fsmonitor-watchman.sample"; @".git\hooks\post-update.sample";
            @".git\hooks\pre-applypatch.sample"; @".git\hooks\pre-commit.sample";
            @".git\hooks\pre-merge-commit.sample"; @".git\hooks\pre-push.sample";
            @".git\hooks\pre-rebase.sample"; @".git\hooks\pre-receive.sample";
            @".git\hooks\prepare-commit-msg.sample";
            @".git\hooks\push-to-checkout.sample"; @".git\hooks\update.sample";
            @".git\info\exclude"; @"assays\est\isa.assay.xlsx"; @"assays\est\README.md";
            @"assays\TestAssay1\isa.assay.xlsx"; @"assays\TestAssay1\README.md";
            @"studies\est\isa.study.xlsx"; @"studies\est\README.md";
            @"studies\MyStudy\isa.study.xlsx"; @"studies\MyStudy\README.md";
            @"studies\TestAssay1\isa.study.xlsx"; @"studies\TestAssay1\README.md";
            @"assays\est\dataset\.gitkeep"; @"assays\est\protocols\.gitkeep";
            @"assays\TestAssay1\dataset\.gitkeep";
            @"assays\TestAssay1\protocols\.gitkeep"; @"studies\est\protocols\.gitkeep";
            @"studies\est\resources\.gitkeep"; @"studies\MyStudy\protocols\.gitkeep";
            @"studies\MyStudy\resources\.gitkeep";
            @"studies\TestAssay1\protocols\.gitkeep";
            @"studies\TestAssay1\resources\.gitkeep"
            |]
            |> Array.map (fun x -> x.Replace(@"\","/"))
            |> Array.sort
        let arc = ARC.fromFilePaths(input)
        Expect.isNone arc.CWL "cwl"
        Expect.isNone arc.ISA "isa"
        let actualFilePaths = arc.FileSystem.Tree.ToFilePaths() |> Array.sort
        Expect.equal actualFilePaths input "isSome fs"
]


let private test_isaFromContracts = testList "read_contracts" [
    testCase "simpleISA" (fun () ->
        let iContract = TestObjects.ISAContracts.SimpleISA.investigationReadContract
        let sContract = TestObjects.ISAContracts.SimpleISA.studyReadContract
        let aContract = TestObjects.ISAContracts.SimpleISA.assayReadContract
        let arc = ARC()
        arc.SetISAFromContracts([|iContract; sContract; aContract|])
        Expect.isSome arc.ISA "isa should be filled out"
        let inv = arc.ISA.Value
        Expect.equal inv.Identifier TestObjects.Investigation.investigationIdentifier "investigation identifier should have been read from investigation contract"

        Expect.equal inv.Studies.Count 2 "should have read two studies"
        let study1 = inv.Studies.[0]
        let study2 = inv.Studies.[1]
        Expect.equal study1.Identifier TestObjects.Study.studyIdentifier "study 1 identifier should have been read from study contract"
        Expect.equal study1.TableCount 8 "study 1 should have the 7 tables from investigation plus one extra. One table should be overwritten."
        Expect.equal study2.TableCount 4 "study 2 should have exactly as many tables as stated in investigation file"
        
        Expect.equal study1.AssayCount 3 "study 1 should have read three assays"
        let assay1 = study1.Assays.[0]
        let assay2 = study1.Assays.[1]
        let assay3 = study1.Assays.[2]
        Expect.equal assay1.Identifier TestObjects.Assay.assayIdentifier "assay 1 identifier should have been read from assay contract"
        Expect.equal assay1.TableCount 1 "assay 1 should have read one table"
        Expect.equal assay2.TableCount 0 "assay 2 should have read no tables"
        Expect.equal assay3.TableCount 0 "assay 3 should have read no tables"
    
    
    )

]

let private test_writeContracts = testList "write_contracts" [
    testCase "empty" (fun _ ->
        let arc = ARC()
        let contracts = arc.GetWriteContracts()
        let contractPathsString = contracts |> Array.map (fun c -> c.Path) |> String.concat ", "
        Expect.equal contracts.Length 5 $"Should contain exactly as much contracts as base folders but contained: {contractPathsString}" 
        Expect.exists contracts (fun c -> c.Path = "workflows/.gitkeep") "Contract for workflows folder missing"
        Expect.exists contracts (fun c -> c.Path = "runs/.gitkeep") "Contract for runs folder missing"
        Expect.exists contracts (fun c -> c.Path = "assays/.gitkeep") "Contract for assays folder missing"
        Expect.exists contracts (fun c -> c.Path = "studies/.gitkeep") "Contract for studies folder missing"
        Expect.exists contracts (fun c -> c.Path = "isa.investigation.xlsx") "Contract for investigation folder missing"
        Expect.exists contracts (fun c -> c.Path = "isa.investigation.xlsx" && c.DTOType.IsSome && c.DTOType.Value = Contract.DTOType.ISA_Investigation) "Contract for investigation exisiting but has wrong DTO type"

    )
    testCase "simpleISA" (fun _ ->
        let inv = ArcInvestigation("MyInvestigation", "BestTitle")
        inv.InitStudy("MyStudy").InitAssay("MyAssay") |> ignore
        let arc = ARC(isa = inv)
        let contracts = arc.GetWriteContracts()
        let contractPathsString = contracts |> Array.map (fun c -> c.Path) |> String.concat ", "
        Expect.equal contracts.Length 13 $"Should contain exactly as much contracts as base folders but contained: {contractPathsString}"

        // Base 
        Expect.exists contracts (fun c -> c.Path = "workflows/.gitkeep") "Contract for workflows folder missing"
        Expect.exists contracts (fun c -> c.Path = "runs/.gitkeep") "Contract for runs folder missing"
        Expect.exists contracts (fun c -> c.Path = "assays/.gitkeep") "Contract for assays folder missing"
        Expect.exists contracts (fun c -> c.Path = "studies/.gitkeep") "Contract for studies folder missing"
        Expect.exists contracts (fun c -> c.Path = "isa.investigation.xlsx") "Contract for investigation folder missing"
        Expect.exists contracts (fun c -> c.Path = "isa.investigation.xlsx" && c.DTOType.IsSome && c.DTOType.Value = Contract.DTOType.ISA_Investigation) "Contract for investigation exisiting but has wrong DTO type"

        // Study folder
        Expect.exists contracts (fun c -> c.Path = "studies/MyStudy/README.md") "study readme missing"
        Expect.exists contracts (fun c -> c.Path = "studies/MyStudy/protocols/.gitkeep") "study protocols folder missing"
        Expect.exists contracts (fun c -> c.Path = "studies/MyStudy/resources/.gitkeep") "study resources folder missing"
        Expect.exists contracts (fun c -> c.Path = "studies/MyStudy/isa.study.xlsx") "study file missing"
        Expect.exists contracts (fun c -> c.Path = "studies/MyStudy/isa.study.xlsx" && c.DTOType.IsSome && c.DTOType.Value = Contract.DTOType.ISA_Study) "study file exisiting but has wrong DTO type"

        // Assay folder
        Expect.exists contracts (fun c -> c.Path = "assays/MyAssay/README.md") "assay readme missing"
        Expect.exists contracts (fun c -> c.Path = "assays/MyAssay/protocols/.gitkeep") "assay protocols folder missing"
        Expect.exists contracts (fun c -> c.Path = "assays/MyAssay/dataset/.gitkeep") "assay dataset folder missing"
        Expect.exists contracts (fun c -> c.Path = "assays/MyAssay/isa.assay.xlsx") "assay file missing"
        Expect.exists contracts (fun c -> c.Path = "assays/MyAssay/isa.assay.xlsx" && c.DTOType.IsSome && c.DTOType.Value = Contract.DTOType.ISA_Assay) "assay file exisiting but has wrong DTO type"

    )
]

let private test_updateFileSystem = testList "update_Filesystem" [
    testCase "empty noChanges" (fun () ->
        let arc = ARC()
        let oldFS = arc.FileSystem.Copy()
        arc.UpdateFileSystem()
        let newFS = arc.FileSystem
        Expect.equal oldFS.Tree newFS.Tree "Tree should be equal"
    )
    testCase "empty addInvestigationWithStudy" (fun () ->
        let arc = ARC()
        let oldFS = arc.FileSystem.Copy()
        let study = ArcStudy("MyStudy")
        let inv = ArcInvestigation("MyInvestigation")
        inv.AddStudy(study)
        arc.ISA <- Some (inv)
        arc.UpdateFileSystem()
        let newFS = arc.FileSystem
        Expect.notEqual oldFS.Tree newFS.Tree "Tree should be unequal"
    )
    testCase "simple noChanges" (fun () ->
        let study = ArcStudy("MyStudy")
        let inv = ArcInvestigation("MyInvestigation")
        inv.AddStudy(study)
        let arc = ARC(isa = inv)
        let oldFS = arc.FileSystem.Copy()   
        arc.UpdateFileSystem()
        let newFS = arc.FileSystem
        Expect.equal oldFS.Tree newFS.Tree "Tree should be equal"
    )
    testCase "simple addAssayToStudy" (fun () ->
        let study = ArcStudy("MyStudy")
        let inv = ArcInvestigation("MyInvestigation")
        inv.AddStudy(study)
        let arc = ARC(isa = inv)
        let oldFS = arc.FileSystem.Copy()   
        let assay = ArcAssay("MyAssay")
        study.AddAssay(assay)
        arc.UpdateFileSystem()
        let newFS = arc.FileSystem
        Expect.notEqual oldFS.Tree newFS.Tree "Tree should be unequal"
    )
]

let main = testList "main" [
    test_model
    test_updateFileSystem
    test_isaFromContracts
    test_writeContracts
]