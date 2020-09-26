open Migrate_parsetree;
open Ast_406;
open Ast_mapper;
open Parsetree;
open Asttypes;
open Longident;

module StringMap = Map.Make(String);

let generateId = value => {
  "_" ++ (value |> Digest.string |> Digest.to_hex |> String.sub(_, 0, 8));
};

let printExpression = (expr: Parsetree.expression) =>
  Printast.expression(0, Format.str_formatter, expr);

let extractLabels = labels => {
  let map = ref(StringMap.empty);
  labels
  |> List.iter(assoc =>
       switch (assoc) {
       | (
           Labelled(key),
           {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
         ) =>
         map := map^ |> StringMap.add(key, value)
       | _ => ()
       }
     );
  map^;
};

let hasIntlAttribute = (items: structure) => {
  items
  |> List.exists(item =>
       switch (item) {
       | {
           pstr_desc: Pstr_attribute(({txt: "intl.messages", _}, PStr([]))),
           _,
         } =>
         true
       | _ => false
       }
     );
};

let replaceIdLabel = (value, labels) => {
  labels
  |> List.fold_left(
       (acc, assoc) =>
         switch (assoc) {
         | (Labelled(key), _) when key == "id" => acc
         | _ as label => List.append(acc, [label])
         },
       [],
     )
  |> List.append([
       (
         Labelled("id"),
         value
         |> generateId
         |> Ast_helper.Const.string
         |> Ast_helper.Exp.constant,
       ),
     ]);
};

let replaceIdRecord =
    (
      value,
      fields: list((Asttypes.loc(Longident.t), Parsetree.expression)),
    ) => {
  fields
  |> List.fold_left(
       (acc, assoc) =>
         switch ((assoc: (Asttypes.loc(Longident.t), Parsetree.expression))) {
         | ({txt: Lident(key), _}, _) when key == "id" => acc
         | _ as label => List.append(acc, [label])
         },
       [],
     )
  |> List.append([
       (
         {txt: Lident("id"), loc: Ast_helper.default_loc.contents},
         value
         |> generateId
         |> Ast_helper.Const.string
         |> Ast_helper.Exp.constant,
       ),
     ]);
};

let extractMessageFromRecord = fields => {
  let map = ref(StringMap.empty);
  fields
  |> List.iter(assoc =>
       switch (assoc) {
       | (
           {txt: Lident(key), _},
           {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
         ) =>
         map := map^ |> StringMap.add(key, value)
       | _ => ()
       }
     );
  map^;
};

let matchesFormattedMessage = ident =>
  switch (ident) {
  | Ldot(Ldot(Lident("ReactIntl"), "FormattedMessage"), "createElement")
  | Ldot(Lident("FormattedMessage"), "createElement") => true
  | _ => false
  };

let expr = (mapper, expr) => {
  switch (expr) {
  // Match (ReactIntl.)FormattedMessage.createElement
  | {
      pexp_desc:
        Pexp_apply(
          {pexp_desc: Pexp_ident({txt, _}), _} as applyExpression,
          labels,
        ),
      _,
    }
      when matchesFormattedMessage(txt) =>
    let labelsMap = extractLabels(labels);
    let defaultMessage = labelsMap |> StringMap.find_opt("defaultMessage");
    let id = labelsMap |> StringMap.find_opt("id");

    switch (id, defaultMessage) {
    | (None, Some(defaultMessage)) =>
      Ast_helper.Exp.apply(
        ~attrs=expr.pexp_attributes,
        ~loc=expr.pexp_loc,
        applyExpression,
        replaceIdLabel(defaultMessage, labels),
      )
    | _ => default_mapper.expr(mapper, expr)
    };

  | _ => default_mapper.expr(mapper, expr)
  };
};

let structure = (mapper, structure) =>
  // Match Module={[@intl.messages]; ...}
  if (hasIntlAttribute(structure)) {
    default_mapper.structure(
      mapper,
      structure
      |> List.map(item =>
           switch (item) {
           | {
               pstr_desc: Pstr_value(Nonrecursive, valueBindings),
               pstr_loc: loc,
             } =>
             valueBindings
             |> List.map(valueBinding =>
                  switch (valueBinding) {
                  | {
                      pvb_pat: {ppat_desc: Ppat_var(_), _} as pattern,
                      pvb_expr:
                        {pexp_desc: Pexp_record(fields, None), _} as expr,
                      _,
                    } =>
                    let map = extractMessageFromRecord(fields);
                    let defaultMessage =
                      map |> StringMap.find_opt("defaultMessage");
                    let id = map |> StringMap.find_opt("id");

                    switch (id, defaultMessage) {
                    | (None, Some(defaultMessage)) =>
                      Ast_helper.Vb.mk(
                        ~attrs=valueBinding.pvb_attributes,
                        ~loc=valueBinding.pvb_loc,
                        pattern,
                        Ast_helper.Exp.record(
                          ~attrs=expr.pexp_attributes,
                          ~loc=expr.pexp_loc,
                          replaceIdRecord(defaultMessage, fields),
                          None,
                        ),
                      )
                    | _ => default_mapper.value_binding(mapper, valueBinding)
                    };
                  | _ => default_mapper.value_binding(mapper, valueBinding)
                  }
                )
             |> Ast_helper.Str.value(~loc, Nonrecursive)
           | _ => default_mapper.structure_item(mapper, item)
           }
         ),
    );
  } else {
    default_mapper.structure(mapper, structure);
  };

let mapper = (_, _) => {...default_mapper, expr, structure};

let () =
  Driver.register(
    ~name="react_intl_auto_id_ppx",
    ~args=[],
    Versions.ocaml_406,
    mapper,
  );
