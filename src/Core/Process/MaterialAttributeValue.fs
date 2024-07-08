﻿namespace ARCtrl.Process

open ARCtrl
open ARCtrl.Helper 

type MaterialAttributeValue = 
    {
        ID : URI option
        Category : MaterialAttribute option
        Value : Value option
        Unit : OntologyAnnotation option
    
    }

    static member make id category value unit : MaterialAttributeValue =
        {
            ID      = id
            Category = category
            Value = value
            Unit = unit         
        }

    static member create(?Id,?Category,?Value,?Unit) : MaterialAttributeValue =
        MaterialAttributeValue.make Id Category Value Unit

    static member empty =
        MaterialAttributeValue.create()

    /// Returns the name of the category as string
    member this.NameText =
        this.Category
        |> Option.map (fun oa -> oa.NameText)
        |> Option.defaultValue ""

    /// Returns the name of the category as string
    member this.TryNameText =
        this.Category
        |> Option.bind (fun oa -> oa.TryNameText)

    member this.ValueText =
        this.Value
        |> Option.map (fun oa ->
            match oa with
            | Value.Ontology oa  -> oa.NameText
            | Value.Float f -> string f
            | Value.Int i   -> string i
            | Value.Name s  -> s
        )
        |> Option.defaultValue ""

    member this.ValueWithUnitText =
        let unit = 
            this.Unit |> Option.map (fun oa -> oa.NameText)
        let v = this.ValueText
        match unit with
        | Some u    -> sprintf "%s %s" v u
        | None      -> v

    member this.MapCategory(f : OntologyAnnotation -> OntologyAnnotation) =
        {this with Category = this.Category |> Option.map (fun p -> p.MapCategory f) }

    member this.SetCategory(c : OntologyAnnotation) =
        {this with Category = 
                            match this.Category with
                            | Some p -> Some (p.SetCategory c)
                            | None -> Some (MaterialAttribute.create(CharacteristicType = c))
        }

    interface IISAPrintable with
        member this.Print() =
            this.ToString()
        member this.PrintCompact() =
            let category = this.Category |> Option.map (fun f -> f.NameText)
            let unit = this.Unit |> Option.map (fun oa -> oa.NameText)
            let value = 
                this.Value
                |> Option.map (fun v ->
                    let s = (v :> IISAPrintable).PrintCompact()
                    match unit with
                    | Some u -> s + " " + u
                    | None -> s
                )
            match category,value with
            | Some category, Some value -> category + ":" + value
            | Some category, None -> category + ":" + "No Value"
            | None, Some value -> value
            | None, None -> ""

    /// Returns the name of the characteristic value as string if it exists
    static member tryGetNameText (mv : MaterialAttributeValue) =
        mv.TryNameText

    /// Returns the name of the characteristic value as string
    static member getNameAsString (mv : MaterialAttributeValue) =
        mv.TryNameText

    /// Returns true if the given name matches the name of the characteristic value
    static member nameEqualsString (name : string) (mv : MaterialAttributeValue) =
        mv.NameText = name

    interface IPropertyValue with
             
        member this.AlternateName() = None

        member this.MeasurementMethod() =
            None

        member this.Description() =
            None

        member this.GetCategory() = 
            this.Category |> Option.bind (fun x -> x.CharacteristicType)

        member this.GetValue() =             
            this.Value

        member this.GetUnit() = 
            this.Unit

        member this.GetAdditionalType() = "MaterialAttributeValue"

    static member createAsPV (alternateName : string option) (measurementMethod : string option) (description : string option) (category : OntologyAnnotation option) (value : Value option) (unit : OntologyAnnotation option) =
        let category = category |> Option.map (fun c -> MaterialAttribute.create(CharacteristicType = c))
        MaterialAttributeValue.create(?Category = category, ?Value = value, ?Unit = unit)

    ///// Returns the value of the characteristic value as string if it exists (with unit)
    //static member tryGetValueAsString (mv : MaterialAttributeValue) =
    //    let unit = mv.Unit |> Option.bind (OntologyAnnotation.tryGetNameAsString)
    //    mv.Value
    //    |> Option.map (fun v ->
    //        let s = v |> Value.toString
    //        match unit with
    //        | Some u -> s + " " + u
    //        | None -> s
    //    )

    ///// Returns the value of the characteristic value as string (with unit)
    //static member getValueAsString (mv : MaterialAttributeValue) =
    //    tryGetValueAsString mv
    //    |> Option.defaultValue ""
