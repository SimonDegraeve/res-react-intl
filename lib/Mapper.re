open Migrate_parsetree;
open Ast_406;
open Ast_mapper;
open Asttypes;
open Parsetree;
open Longident;

let generateId = value => {
  "_" ++ (value |> Digest.string |> Digest.to_hex |> String.sub(_, 0, 8));
};

let replaceIdFromLabels = (~loc, labels) => {
  let id = ref(`None);

  let labels =
    labels
    |> List.fold_left(
         (acc, assoc) =>
           switch (assoc) {
           | (
               Labelled(key),
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             )
               when key == "id" =>
             id.contents = `Id(value);
             acc;
           | (
               Labelled(key),
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             ) as label
               when key == "defaultMessage" =>
             id.contents = (
               switch (id.contents) {
               | `None => `Message(value)
               | value => value
               }
             );
             List.append(acc, [label]);
           | label => List.append(acc, [label])
           },
         [],
       );

  let hasId =
    labels
    |> List.exists(label =>
         switch (label) {
         | (Labelled(key), _) when key == "id" => true
         | _ => false
         }
       );

  hasId
    ? labels
    : labels
      |> List.append(
           {
             [
               (
                 Labelled("id"),
                 (
                   switch (id.contents) {
                   | `Message(value) => value |> generateId
                   | `Id(value) => value
                   | `None =>
                     raise(
                       Location.Error(
                         Location.error(
                           ~loc,
                           "Missing id for ReactIntl message",
                         ),
                       ),
                     )
                   }
                 )
                 |> Ast_helper.Const.string
                 |> Ast_helper.Exp.constant,
               ),
             ];
           },
         );
};

let replaceIdFromRecord = (~loc, fields) => {
  let id = ref(`None);

  let fields =
    fields
    |> List.fold_left(
         (acc, assoc) =>
           switch (assoc) {
           | (
               {txt: Lident(key), _},
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             )
               when key == "id" =>
             id.contents = `Id(value);
             acc;
           | (
               {txt: Lident(key), _},
               {pexp_desc: Pexp_constant(Pconst_string(value, _)), _},
             ) as field
               when key == "defaultMessage" =>
             id.contents = (
               switch (id.contents) {
               | `None => `Message(value)
               | value => value
               }
             );
             List.append(acc, [field]);
           | field => List.append(acc, [field])
           },
         [],
       );

  let hasId =
    fields
    |> List.exists(field =>
         switch (field) {
         | ({txt: Lident(key), _}, _) when key == "id" => true
         | _ => false
         }
       );

  hasId
    ? fields
    : fields
      |> List.append([
           (
             {txt: Lident("id"), loc: Ast_helper.default_loc.contents},
             (
               switch (id.contents) {
               | `Message(value) => value |> generateId
               | `Id(value) => value
               | `None =>
                 raise(
                   Location.Error(
                     Location.error(~loc, "Missing id for ReactIntl message"),
                   ),
                 )
               }
             )
             |> Ast_helper.Const.string
             |> Ast_helper.Exp.constant,
           ),
         ]);
};

let extractMessageFromLabels = (~loc, labels, callback) => {
  let labels = labels |> replaceIdFromLabels(~loc);

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

let extractMessageFromRecord = (~loc, fields, callback) => {
  let fields = fields |> replaceIdFromRecord(~loc);

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
             extractMessageFromRecord(expr.pexp_loc, fields, callback),
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
        pexp_loc: loc,
        _,
      }
        when matchesFormattedMessage(txt) =>
      Ast_helper.Exp.apply(
        ~attrs=expr.pexp_attributes,
        ~loc=expr.pexp_loc,
        applyExpr,
        extractMessageFromLabels(~loc, labels, callback),
      )

    | _ => default_mapper.expr(mapper, expr)
    };
  },
};
