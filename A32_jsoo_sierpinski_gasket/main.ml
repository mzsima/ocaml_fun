open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type point = {x : float; y : float}

class triangle t l =
  object
    val color = Js.string "black";

    method draw (ctx : canvasCtx) theta inverse = 
      let r = l *. 0.5 in
      let rec draw_tri_path n th = match n with
        | 0 -> () 
        | _ ->
          let x = (t.x) +. (cos th) *. r in
          let y = (t.y +. (inverse *. r)) +. (sin th) *. r in
          ctx##lineTo x y ;
          draw_tri_path (n-1) (th +. pi *. (2. /. 3.))
      in
      ctx##beginPath ;
      ctx##.lineWidth := 5.;
      ctx##.fillStyle := color ;
      ctx##moveTo t.x t.y ;
      let _ = draw_tri_path 3 theta in
      ctx##fill ;
      ()
end

let rec create_holes t l c = 
  if l < 10. then ()
  else (
    let tri = new triangle t l in
    tri#draw c ( pi /. 2.) (-. 1.) ;
    create_holes {x = t.x -. l *. 0.435 ; y = t.y } (l *. 0.5) c;
    create_holes {x = t.x +. l *. 0.435 ; y = t.y } (l *. 0.5) c;
    create_holes {x = t.x; y = t.y -. l *. 0.75} (l *. 0.5) c
  )
let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  c##.globalCompositeOperation := Js.string "xor" ;
  let w, h = (float canvas##.width), (float canvas##.height) in
  let tri = new triangle {x = w *. 0.5; y = h *. 0.1 } w in
  tri#draw c ( pi /. 6.) (1.); 
  create_holes {x = w *. 0.5; y = h *. 0.85 } (w *. 0.5) c ;
  ()

let () = Js.export "init" init