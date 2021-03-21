namespace ISADotNet.API

open ISADotNet

module ProcessParameterValue =

    /// Returns the name of the paramater value as string if it exists
    let tryGetNameAsString (pv : ProcessParameterValue) =
        pv.Category
        |> Option.bind (ProtocolParameter.tryGetNameAsString)

    /// Returns the name of the paramater value as string
    let getNameAsString (pv : ProcessParameterValue) =
        tryGetNameAsString pv
        |> Option.defaultValue ""

    /// Returns true if the given name matches the name of the parameter value
    let nameEqualsString (name : string) (pv : ProcessParameterValue) =
        match pv.Category with
        | Some oa -> ProtocolParameter.nameEqualsString name oa
        | None -> false


/// Functions for handling the ProcessInput Type
module ProcessInput =

    /// Returns name of processInput
    let getName (pi : ProcessInput) =
        match pi with
        | ProcessInput.Sample s     -> s.Name
        | ProcessInput.Source s     -> s.Name
        | ProcessInput.Material m   -> m.Name
        | ProcessInput.Data d       -> d.Name

    /// Returns true, if given name equals name of processInput
    let nameEquals (name : string) (pi : ProcessInput) =
        match pi with
        | ProcessInput.Sample s     -> s.Name = (Some name)
        | ProcessInput.Source s     -> s.Name = (Some name)
        | ProcessInput.Material m   -> m.Name = (Some name)
        | ProcessInput.Data d       -> d.Name = (Some name)

    /// Returns true, if Process Input is Sample
    let isSample (pi : ProcessInput) =
        match pi with
        | ProcessInput.Sample _ -> true
        | _ -> false

    /// Returns true, if Process Input is Source
    let isSource (pi : ProcessInput) =
        match pi with
        | ProcessInput.Source _ -> true
        | _ -> false

    /// Returns true, if Process Input is Data
    let isData (pi : ProcessInput) =
        match pi with
        | ProcessInput.Data _ -> true
        | _ -> false

    /// Returns true, if Process Input is Material
    let isMaterial (pi : ProcessInput) =
        match pi with
        | ProcessInput.Material _ -> true
        | _ -> false

    /// If given process input is a sample, returns it, else returns None
    let trySample (pi : ProcessInput) =
        match pi with
        | ProcessInput.Sample s -> Some s
        | _ -> None

    /// If given process input is a source, returns it, else returns None
    let trySource (pi : ProcessInput) =
        match pi with
        | ProcessInput.Source s -> Some s
        | _ -> None

    /// If given process input is a data, returns it, else returns None
    let tryData (pi : ProcessInput) =
        match pi with
        | ProcessInput.Data d -> Some d
        | _ -> None

    /// If given process input is a material, returns it, else returns None
    let tryMaterial (pi : ProcessInput) =
        match pi with
        | ProcessInput.Material m -> Some m
        | _ -> None

    /// If given process input contains characteristics, returns them
    let tryGetCharacteristics (pi : ProcessInput) =
        match pi with
        | ProcessInput.Sample s     -> s.Characteristics
        | ProcessInput.Source s     -> s.Characteristics
        | ProcessInput.Material m   -> m.Characteristics
        | ProcessInput.Data _       -> None

/// Functions for handling the ProcessOutput Type
module ProcessOutput =

    /// Returns name of processOutput
    let getName (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample s     -> s.Name
        | ProcessOutput.Material m   -> m.Name
        | ProcessOutput.Data d       -> d.Name

    /// Returns true, if given name equals name of processOutput
    let nameEquals (name : string) (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample s     -> s.Name = (Some name)
        | ProcessOutput.Material m   -> m.Name = (Some name)
        | ProcessOutput.Data d       -> d.Name = (Some name)

    /// Returns true, if Process Output is Sample
    let isSample (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample _ -> true
        | _ -> false

    /// Returns true, if Process Output is Data
    let isData (po : ProcessOutput) =
        match po with
        | ProcessOutput.Data _ -> true
        | _ -> false

    /// Returns true, if Process Output is Material
    let isMaterial (po : ProcessOutput) =
        match po with
        | ProcessOutput.Material _ -> true
        | _ -> false

    /// If given process output is a sample, returns it, else returns None
    let trySample (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample s -> Some s
        | _ -> None

    /// If given process output is a data, returns it, else returns None
    let tryData (po : ProcessOutput) =
        match po with
        | ProcessOutput.Data d -> Some d
        | _ -> None

    /// If given process output is a material, returns it, else returns None
    let tryMaterial (po : ProcessOutput) =
        match po with
        | ProcessOutput.Material m -> Some m
        | _ -> None


    /// If given process output contains characteristics, returns them
    let tryGetCharacteristics (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample s     -> s.Characteristics
        | ProcessOutput.Material m   -> m.Characteristics
        | ProcessOutput.Data _       -> None

    /// If given process output contains factors, returns them
    let tryGetFactorValues (po : ProcessOutput) =
        match po with
        | ProcessOutput.Sample s     -> s.FactorValues
        | ProcessOutput.Material _   -> None
        | ProcessOutput.Data _       -> None

/// Functions for handling ISA Processes
module Process =

    /// Returns the parameters describing the process
    let getParameters (p: Process) =
        match p.ParameterValues with
        | Some paramValues ->
            paramValues
            |> List.choose (fun pv -> pv.Category)
        | None -> []

    /// If the process implements the given parameter, return the list of input files together with their according parameter values of this parameter
    let tryGetInputsWithParameterBy (predicate : ProtocolParameter -> bool) (p : Process) =
        match p.ParameterValues with
        | Some paramValues ->
            match paramValues |> List.tryFind (fun pv -> Option.defaultValue ProtocolParameter.empty pv.Category |> predicate ) with
            | Some paramValue ->
                p.Inputs
                |> Option.map (List.map (fun i -> i,paramValue))
            | None -> None
        | None -> None

    
    /// If the process implements the given parameter, return the list of output files together with their according parameter values of this parameter
    let tryGetOutputsWithParameterBy (predicate : ProtocolParameter -> bool) (p : Process) =
        match p.ParameterValues with
        | Some paramValues ->
            match paramValues |> List.tryFind (fun pv -> Option.defaultValue ProtocolParameter.empty pv.Category |> predicate ) with
            | Some paramValue ->
                p.Outputs
                |> Option.map (List.map (fun i -> i,paramValue))
            | None -> None
        | None -> None

    /// Returns the characteristics of the samples of the process
    let getCharacteristics (p : Process) =
        let materialAttributesOfValues (mvs : (MaterialAttributeValue list) Option) = 
            mvs |> Option.defaultValue [] |> List.choose (fun mv -> mv.Category)
        p.Inputs |> Option.defaultValue [] |> List.collect (ProcessInput.tryGetCharacteristics >> materialAttributesOfValues)
        |> List.append (p.Outputs |> Option.defaultValue [] |> List.collect (ProcessOutput.tryGetCharacteristics >> materialAttributesOfValues))
        |> List.distinct

    /// If the process implements the given characteristic, return the list of input files together with their according characteristic values of this characteristic
    let tryGetInputsWithCharacteristicBy (predicate : MaterialAttribute -> bool) (p : Process) =
        match p.Inputs with
        | Some is ->
            is
            |> List.choose (fun i ->
                ProcessInput.tryGetCharacteristics i
                |> Option.defaultValue []
                |> List.tryPick (fun mv -> 
                    match mv.Category with
                    | Some m when predicate m -> Some (i,mv)
                    | _ -> None

                )
            )
            |> Option.fromValueWithDefault []
        | None -> None

    /// If the process implements the given characteristic, return the list of output files together with their according characteristic values of this characteristic
    let tryGetOutputsWithCharacteristicBy (predicate : MaterialAttribute -> bool) (p : Process) =
        match p.Outputs with
        | Some os ->
            os
            |> List.choose (fun o ->
                ProcessOutput.tryGetCharacteristics o
                |> Option.defaultValue []
                |> List.tryPick (fun mv -> 
                    match mv.Category with
                    | Some m when predicate m -> Some (o,mv)
                    | _ -> None

                )
            )
            |> Option.fromValueWithDefault []
        | None -> None

    /// Returns the factors of the samples of the process
    let getFactors (p : Process) =
        let factorsOfValues (fvs : (FactorValue list) Option) = 
            fvs |> Option.defaultValue [] |> List.choose (fun fv -> fv.Category)
        p.Outputs |> Option.defaultValue [] |> List.collect (ProcessOutput.tryGetFactorValues >> factorsOfValues)
        |> List.distinct

    /// If the process implements the given factor, return the list of output files together with their according factor values of this factor
    let tryGetOutputsWithFactorBy (predicate : Factor -> bool) (p : Process) =
        match p.Outputs with
        | Some os ->
            os
            |> List.choose (fun o ->
                ProcessOutput.tryGetFactorValues o
                |> Option.defaultValue []
                |> List.tryPick (fun mv -> 
                    match mv.Category with
                    | Some m when predicate m -> Some (o,mv)
                    | _ -> None

                )
            )
            |> Option.fromValueWithDefault []
        | None -> None