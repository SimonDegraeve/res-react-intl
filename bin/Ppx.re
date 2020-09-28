open Migrate_parsetree;
open Ast_406;

let mapper = Mapper.getMapper(_ => ());

let _ =
  Driver.register(~name="ReactIntl", ~args=[], Versions.ocaml_406, (_, _) =>
    mapper
  );

let _ = Driver.run_as_ppx_rewriter();
