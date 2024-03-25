﻿namespace ARCtrl.Json.ROCrateContext

open Thoth.Json.Core

module PropertyValue =

  let context_jsonvalue =
    Encode.object [
      "sdo", Encode.string "http://schema.org/"
      "additionalType", Encode.string "sdo:additionalType"
      "ProcessParameterValue", Encode.string "sdo:PropertyValue"

      "category", Encode.string "sdo:name"
      "categoryCode", Encode.string "sdo:propertyID"
      "value", Encode.string "sdo:value"
      "valueCode", Encode.string "sdo:valueReference"
      "unit", Encode.string "sdo:unitText"
      "unitCode", Encode.string "sdo:unitCode"
      "comments", Encode.string "sdo:disambiguatingDescription"
    ]