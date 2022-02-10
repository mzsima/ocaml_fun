open Js_of_ocaml
open Graphics_js

module Html = Dom_html

let pi = acos (-1.)

let x = ref 0.0 
let y = ref 0.0 

let setup canvas =
  x := (float canvas##.width) /. 2.0 ;
  y := (float canvas##.height) -. 30. ;
  ()

let create_rect c = 
  c##beginPath ;
  c##rect 20.0 40.0 50.0 50.0 ;
  c##.fillStyle := Js.string "#FF0000" ;
  c##fill ;
  c##closePath ; 

  c##beginPath ;
  c##arc 240.0 160.0 20.0 0.0 (pi *. 2.0) Js._false ;
  c##.fillStyle := Js.string "green" ;
  c##fill ;
  c##closePath ; 

  c##beginPath ;
  c##rect 160. 10. 100. 40. ;
  c##.strokeStyle := Js.string "rgba(0, 0, 255, 0.5)" ;
  c##stroke ;
  c##closePath ;
  ()

let drawBall c = 
  c##beginPath ;
  c##arc !x !y 30. 0. (pi *. 2.0) Js._true;
  c##.fillStyle := Js.string "#0095DD" ;
  c##fill ;
  c##closePath ;  
  ()

let draw canvas c = fun () ->
  c##clearRect 0. 200. (float canvas##.width) (float canvas##.height) ;
  drawBall c ;
  x := !x +. 2.0 ;
  y := !y -. 2.0 ;
  ()

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  setup canvas ;
  let c = canvas##getContext Html._2d_ in
  create_rect c ;
  let _ = Dom_html.window##setInterval (Js.wrap_callback (draw canvas c)) 10. in
  ()

let () = Js.export "init" init