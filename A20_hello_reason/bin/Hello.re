Console.log("Running Test Program:");

let hello = () =>
  Pastel.(
    <Pastel>
      <Pastel color=Red> "Re" </Pastel>
      <Pastel color=White> "ason" </Pastel>
      "!"
    </Pastel>
  );

let () = print_endline(hello());
