open Migrate_parsetree;
open Ast_406;
open Ast_mapper;
open Asttypes;
open Parsetree;
open Longident;

let generateId = value => {
  "_" ++ (value |> Digest.string |> Digest.to_hex |> String.sub(_, 0, 8));
};

let printExpression = (expr: expression) =>
  Printast.expression(0, Format.str_formatter, expr);

let replaceIdFromLabels = labels => {
  let id = ref("");
  labels
  |> List.fold_left(
       (acc, assoc) =>
         switch (assoc) {
         | (
             Labelled(key),
             {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
           )
             when key == "id" =>
           id.contents = value;
           acc;
         | (
             Labelled(key),
             {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
           ) as label
             when key == "defaultMessage" =>
           id.contents = id.contents == "" ? value : id.contents;
           List.append(acc, [label]);
         | label => List.append(acc, [label])
         },
       [],
     )
  |> List.append(
       {
         [
           (
             Labelled("id"),
             id.contents
             |> generateId
             |> Ast_helper.Const.string
             |> Ast_helper.Exp.constant,
           ),
         ];
       },
     );
};

let replaceIdFromRecord =
    (fields: list((Asttypes.loc(Longident.t), expression))) => {
  let id = ref("");

  fields
  |> List.fold_left(
       (acc, assoc) =>
         switch ((assoc: (Asttypes.loc(Longident.t), expression))) {
         | (
             {txt: Lident(key), _},
             {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
           )
             when key == "id" =>
           id.contents = value;
           acc;
         | (
             {txt: Lident(key), _},
             {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
           ) as field
             when key == "defaultMessage" =>
           id.contents = id.contents == "" ? value : id.contents;
           List.append(acc, [field]);
         | field => List.append(acc, [field])
         },
       [],
     )
  |> List.append([
       (
         {txt: Lident("id"), loc: Ast_helper.default_loc.contents},
         id.contents
         |> generateId
         |> Ast_helper.Const.string
         |> Ast_helper.Exp.constant,
       ),
     ]);
};

let extractMessageFromLabels = (labels, callback) => {
  let labels = labels |> replaceIdFromLabels;

  let map =
    labels
    |> List.fold_left(
         (map, assoc) =>
           switch (assoc) {
           | (
               Labelled(key),
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             ) =>
             map |> StringMap.add(key, value)
           | _ => map
           },
         StringMap.empty,
       );

  switch (Message.fromStringMap(map)) {
  | Some(value) => callback(value)
  | None => ()
  };

  labels;
};

let extractMessageFromRecord = (fields, callback) => {
  let fields = fields |> replaceIdFromRecord;

  let map =
    fields
    |> List.fold_left(
         (map, field) =>
           switch (field) {
           | (
               {txt: Lident(key), _},
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             ) =>
             map |> StringMap.add(key, value)
           | _ => map
           },
         StringMap.empty,
       );

  switch (Message.fromStringMap(map)) {
  | Some(value) => callback(value)
  | None => ()
  };

  fields;
};

let hasIntlAttribute = (items: structure) =>
  items
  |> List.exists(item =>
       switch (item) {
       | {pstr_desc: Pstr_attribute(({txt: "intl.messages", _}, _)), _} =>
         true
       | _ => false
       }
     );

let extractMessagesFromValueBindings =
    (mapper, valueBindings: list(value_binding), callback) =>
  valueBindings
  |> List.map(valueBinding =>
       switch (valueBinding) {
       | {
           pvb_pat: {ppat_desc: Ppat_var(_), _} as pattern,
           pvb_expr: {pexp_desc: Pexp_record(fields, None), _} as expr,
           _,
         } =>
         Ast_helper.Vb.mk(
           ~attrs=valueBinding.pvb_attributes,
           ~loc=valueBinding.pvb_loc,
           pattern,
           Ast_helper.Exp.record(
             ~attrs=expr.pexp_attributes,
             ~loc=expr.pexp_loc,
             extractMessageFromRecord(fields, callback),
             None,
           ),
         )
       | _ => default_mapper.value_binding(mapper, valueBinding)
       }
     );

let extractMessagesFromModule = (mapper, items: structure, callback) =>
  if (hasIntlAttribute(items)) {
    default_mapper.structure(
      mapper,
      items
      |> List.map(item =>
           switch (item) {
           | {
               pstr_desc: Pstr_value(Nonrecursive, valueBindings),
               pstr_loc: loc,
             } =>
             Ast_helper.Str.value(
               ~loc,
               Nonrecursive,
               extractMessagesFromValueBindings(
                 mapper,
                 valueBindings,
                 callback,
               ),
             )
           | _ => default_mapper.structure_item(mapper, item)
           }
         ),
    );
  } else {
    default_mapper.structure(mapper, items);
  };

let matchesFormattedMessage = ident =>
  switch (ident) {
  | Ldot(Ldot(Lident("ReactIntl"), "FormattedMessage"), "createElement")
  | Ldot(Lident("FormattedMessage"), "createElement") => true
  | _ => false
  };

let getMapper = (callback: Message.t => unit): mapper => {
  ...default_mapper,

  // Match records in modules with [@intl.messages]
  // (structure is the module body - either top level or of a submodule)
  structure: (mapper, structure) => {
    extractMessagesFromModule(mapper, structure, callback);
  },

  expr: (mapper, expr) => {
    switch (expr) {
    // Match (ReactIntl.)FormattedMessage.createElement
    | {
        pexp_desc:
          Pexp_apply(
            {pexp_desc: Pexp_ident({txt, _}), _} as applyExpr,
            labels,
          ),
        _,
      }
        when matchesFormattedMessage(txt) =>
      Ast_helper.Exp.apply(
        ~attrs=expr.pexp_attributes,
        ~loc=expr.pexp_loc,
        applyExpr,
        extractMessageFromLabels(labels, callback),
      )

    | _ => default_mapper.expr(mapper, expr)
    };
  },
};
