namespace ARCtrl.Json

open Thoth.Json.Core

open ARCtrl

module VariableMeasured = 

    module ROCrate =
    
        let encoder : DataContext -> Json= 
            fun (dc : DataContext) ->
                let subjectOf = 
                PropertyValue.ROCrate.encoder dc
                |> Encode.addPropertyToObject "subjectOf"

        let decoder : Decoder<DataContext> =
            PropertyValue.ROCrate.decoder<DataContext> DataContext.createAsPV
