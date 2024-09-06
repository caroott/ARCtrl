namespace ARCtrl.ROCrate

open DynamicObj
open Fable.Core

///
[<AttachMembers>]
type LabProcess(
    id: string,
    name,
    agent,
    object,
    result,
    ?additionalType,
    ?executesLabProtocol,
    ?parameterValue,
    ?endTime,
    ?disambiguatingDescription
) as this =
    inherit ROCrateObject(id = id, schemaType = "bioschemas.org/LabProcess", ?additionalType = additionalType)
    do
        DynObj.setValue this (nameof name) name
        DynObj.setValue this (nameof agent) agent
        DynObj.setValue this (nameof object) object
        DynObj.setValue this (nameof result) result

        DynObj.setValueOpt this (nameof executesLabProtocol) executesLabProtocol
        DynObj.setValueOpt this (nameof parameterValue) parameterValue
        DynObj.setValueOpt this (nameof endTime) endTime
        DynObj.setValueOpt this (nameof disambiguatingDescription) disambiguatingDescription

    member this.GetName() = DynObj.tryGetValue this (nameof name)  |> Option.get
    static member getName = fun (lp: LabProcess) -> lp.GetName()

    member this.GetAgent() = DynObj.tryGetTypedValue<string> (nameof agent) this |> Option.get
    static member getAgent = fun (lp: LabProcess) -> lp.GetAgent()

    member this.GetObject() = DynObj.tryGetTypedValue<string> (nameof object) this |> Option.get
    static member getObject = fun (lp: LabProcess) -> lp.GetObject()

    member this.GetResult() = DynObj.tryGetTypedValue<string> (nameof result) this |> Option.get
    static member getResult = fun (lp: LabProcess) -> lp.GetResult()
