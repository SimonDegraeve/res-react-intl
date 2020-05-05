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

let extractLabels = labels => {
  let map = ref(StringMap.empty);
  labels
  |> List.iter(assoc =>
       switch (assoc) {
       | (
           Labelled(key),
           {
             pexp_desc: Pexp_constant(Pconst_string(value, _)),
             pexp_loc: _,
             pexp_attributes: _,
           },
         ) =>
         map := map^ |> StringMap.add(key, value)
       | _ => ()
       }
     );
  map^;
};

let replaceIdLabel = (value, labels) => {
  labels
  |> List.fold_left(
       (acc, assoc) =>
         switch (assoc) {
         | (Labelled(key), {pexp_desc: _, pexp_loc: _, pexp_attributes: _})
             when key == "id" => acc
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

let matchesFormattedMessage = ident =>
  switch (ident) {
  | Ldot(Ldot(Lident("ReactIntl"), "FormattedMessage"), "createElement")
  | Ldot(Lident("FormattedMessage"), "createElement") => true
  | _ => false
  };

let expr = (mapper, e) => {
  switch (e.pexp_desc) {
  /* Match (ReactIntl.)FormattedMessage.createElement */
  | Pexp_apply(
      {pexp_desc: Pexp_ident({txt, _}), pexp_loc: _, pexp_attributes: _} as applyExpression,
      labels,
    )
      when matchesFormattedMessage(txt) =>
    let labelsMap = extractLabels(labels);

    let defaultMessage = labelsMap |> StringMap.find_opt("defaultMessage");
    let id = labelsMap |> StringMap.find_opt("id");

    switch (id, defaultMessage) {
    | (None, Some(defaultMessage)) =>
      Ast_helper.Exp.apply(
        ~attrs=e.pexp_attributes,
        ~loc=e.pexp_loc,
        applyExpression,
        replaceIdLabel(defaultMessage, labels),
      )
    | _ => default_mapper.expr(mapper, e)
    };

  | _ => default_mapper.expr(mapper, e)
  };
};

let mapper = (_, _) => {...default_mapper, expr};

let () =
  Driver.register(
    ~name="react_intl_auto_id_ppx",
    ~args=[],
    Versions.ocaml_406,
    mapper,
  );