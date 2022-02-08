open Js_of_ocaml
open Graphics_js

module Html = Dom_html

let pi = acos (-1.)

let setup_ctx c = 
  c##.fillStyle := (Js.string "rgb(255, 123, 123)") ;
  c##.strokeStyle := (Js.string "rgb(255, 255, 255)") ;
  c##.lineWidth := 2.0 ;
  c##translate 250.0 150.0 ;
  c##rotate (pi *. 0.125) ;
  ()

let create_rect c n x' y' = 
  let x = (float_of_int (n / 10)) *. 40.0 in
  let y = (float_of_int (n mod 10)) *. 40.0 in
  c##strokeRect x y 40.0 40.0 ;
  c##fillRect (x +. x' +. 2.) (y +. y' +. 2.) 16.0 16.0 ;
  ()

let rec calc c lst = match lst with 
  [] -> ()
  | first :: rest -> 
    if first mod 2 = 0 then create_rect c first 0.0 0.0 ;
    if first mod 3 = 0 then create_rect c first 20.0 0.0 ;
    if first mod 5 = 0 then create_rect c first 00.0 20.0 ;
    if first mod 7 = 0 then create_rect c first 20.0 20.0 ;
    calc c rest ;
    ()
    

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
    let c = canvas##getContext Html._2d_ in
      setup_ctx c ;
      calc c (List.init 100 (fun x->x)) ;
    ()

let () = Js.export "init" init