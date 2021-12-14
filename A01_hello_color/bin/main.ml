open Base
open Stdio
type basic_color = 
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
  | Black -> 0 | Red      -> 1 | Green  -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta  -> 5 | Cyan   -> 6 | White  -> 7

let color_by_nymber number text =
  Printf.sprintf "\027[38;5;%dm%s\027[0m" number text

let redH = color_by_nymber (basic_color_to_int Red) "H"
let greenE = color_by_nymber (basic_color_to_int Green) "E"
let yellowL = color_by_nymber (basic_color_to_int Yellow) "L"
let blueL = color_by_nymber (basic_color_to_int Blue) "L"
let magentaO = color_by_nymber (basic_color_to_int Magenta) "O"
let cyanWorld = color_by_nymber (basic_color_to_int Cyan) "WORLD"

let () = printf "%s%s%s%s%s %s!\n" redH greenE yellowL blueL magentaO cyanWorld
