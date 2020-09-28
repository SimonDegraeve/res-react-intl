let extract = (~duplicatesAllowed, paths) =>
  try({
    let messages = Extractor.extract(~duplicatesAllowed, paths);

    `List(messages |> List.map(Message.toJson))
    |> Yojson.Basic.pretty_to_channel(stdout);
    print_newline();
  }) {
  | Extractor.PathNotFound(path) =>
    Printf.eprintf("Error: file or directory does not exist: %s\n", path);
    exit(1);

  | Extractor.DuplicateMessageId(id) =>
    Printf.eprintf("Error: duplicate message id: %s\n", id);
    exit(2);

  | Extractor.DefaultMessageNotMatching(id) =>
    Printf.eprintf(
      "Error: duplicate message id: %s with different default messages\n",
      id,
    );
    exit(3);

  | exn =>
    Printf.eprintf("Unexpected error: %s\n", Printexc.to_string(exn));
    exit(10);
  };

type options = {
  paths: list(string),
  duplicatesAllowed: bool,
};

let run = () => {
  let options = ref({paths: [], duplicatesAllowed: false});

  let processInputFilename = filename =>
    options := {...options^, paths: [filename, ...options^.paths]};
  let allowDuplicates = () =>
    options := {...options^, duplicatesAllowed: true};

  let args = [
    (
      "--allow-duplicates",
      Arg.Unit(allowDuplicates),
      "allows messages with identical `id` props if `defaultMessage` props are identical as well",
    ),
  ];

  let usage = "Usage: " ++ Sys.argv[0] ++ " [path...]";

  Arg.parse(args, processInputFilename, usage);

  switch (options^) {
  | {paths: []} => Arg.usage(args, usage)
  | {paths, duplicatesAllowed} => extract(~duplicatesAllowed, paths)
  };
};

run();
