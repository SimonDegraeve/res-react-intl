open Ast_iterator;
open Asttypes;
open Parsetree;
open Longident;

let generateId = value => {
  "_" ++ (value |> Digest.string |> Digest.to_hex |> String.sub(_, 0, 8));
};

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
         | _ as label => List.append(acc, [label])
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
    (fields: list((Asttypes.loc(Longident.t), Parsetree.expression))) => {
  let id = ref("");

  fields
  |> List.fold_left(
       (acc, assoc) =>
         switch ((assoc: (Asttypes.loc(Longident.t), Parsetree.expression))) {
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
             when key == "id" =>
           id.contents = id.contents == "" ? value : id.contents;
           List.append(acc, [field]);
         | _ as field => List.append(acc, [field])
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

let extractMessageFromLabels = (callback, labels) => {
  let map =
    labels
    |> replaceIdFromLabels
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
};

let extractMessageFromRecord = (callback, fields) => {
  let map =
    fields
    |> replaceIdFromRecord
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
    (callback, valueBindings: list(value_binding)) =>
  valueBindings
  |> List.iter(valueBinding =>
       switch (valueBinding) {
       | {
           pvb_pat: {ppat_desc: Ppat_var(_), _},
           pvb_expr: {pexp_desc: Pexp_record(fields, None), _},
           _,
         } =>
         extractMessageFromRecord(callback, fields)
       | _ => ()
       }
     );

let extractMessagesFromModule = (callback, items: structure) =>
  if (hasIntlAttribute(items)) {
    items
    |> List.iter(item =>
         switch (item) {
         | {pstr_desc: Pstr_value(Nonrecursive, valueBindings), _} =>
           extractMessagesFromValueBindings(callback, valueBindings)
         | _ => ()
         }
       );
  };

let matchesFormattedMessage = ident =>
  switch (ident) {
  | Ldot(Ldot(Lident("ReactIntl"), "FormattedMessage"), "createElement")
  | Ldot(Lident("FormattedMessage"), "createElement") => true
  | _ => false
  };

let getIterator = (callback: Message.t => unit): Ast_iterator.iterator => {
  ...default_iterator,

  // Match records in modules with [@intl.messages]
  // (structure is the module body - either top level or of a submodule)
  structure: (iterator, structure) => {
    extractMessagesFromModule(callback, structure);
    default_iterator.structure(iterator, structure);
  },

  expr: (iterator, expr) => {
    switch (expr) {
    // Match (ReactIntl.)FormattedMessage.createElement
    | {
        pexp_desc: Pexp_apply({pexp_desc: Pexp_ident({txt, _}), _}, labels),
        _,
      }
        when matchesFormattedMessage(txt) =>
      extractMessageFromLabels(callback, labels)

    | _ => ()
    };

    default_iterator.expr(iterator, expr);
  },
};
