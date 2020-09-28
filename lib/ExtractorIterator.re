open Ast_iterator;
open Asttypes;
open Parsetree;
open Longident;

let generateId = value => {
  "_" ++ (value |> Digest.string |> Digest.to_hex |> String.sub(_, 0, 8));
};

let extractMessageFromLabels = (callback, labels) => {
  let map =
    labels
    |> List.fold_left(
         (map, assoc) =>
           switch (assoc) {
           | (
               Asttypes.Labelled(key),
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
