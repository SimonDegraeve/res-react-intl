open Migrate_parsetree;

let mapper = Mapper.getMapper(_ => ());

let _ =
  Driver.register(~name="ReactIntl", ~args=[], Versions.ocaml_current, (_, _) =>
    mapper
  );

let _ = Driver.run_as_ppx_rewriter();
