open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type model = { w: float; h: float; data: float list } 

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type point = {x : float; y : float}

class bar_chart model =
  object

    method draw (ctx : canvasCtx)  = 
      let pad = model.w *. 0.1 in

      ctx##beginPath ;
      ctx##.lineWidth := 1.;
      ctx##.strokeStyle := Js.string "#8998ad" ;
      ctx##.fillStyle := Js.string "#2b3d5b" ;
      ctx##fillRect 0. 0. model.w model.h ;

      (* axis *)
      let stroke_line p0 p1 = 
        ctx##moveTo p0.x p0.y ;
        ctx##lineTo p1.x p1.y ;
        ctx##stroke 
      in
      stroke_line { x=pad; y=pad } {x=pad; y=model.h -. pad } ;
      stroke_line { x=pad; y=model.h -. pad } {x=model.w -. pad; y=model.h -. pad } ; 

      let rec draw_label_x dx y0 y1 x =
        if x < (model.w -. pad -. 10.) then (
          stroke_line {x; y=y0} {x; y=y1};
          draw_label_x dx y0 y1 (x +. dx)
          ) 
      in
      let rec draw_label_y dy x0 x1 y =
        if y < (model.h -. pad -. 10.) then (
          stroke_line {x=x0; y} {x=x1; y};
          draw_label_y dy x0 x1 (y +. dy)
          ) 
      in
      let dx = (model.w -. pad *. 2.) /. 5.  in
      draw_label_x dx (model.h -. pad +. 16.) (model.h -. pad) (pad +. dx *. 0.5) ;
      draw_label_y 40. (pad -. 16.) pad (pad +. 10.) ;

      let rec draw_bar dx x data = match data with 
        | [] -> ()
        | head :: rest ->  
          ctx##fillRect (x +. 30.) (model.h -. pad) (dx -. 60.) (-. head *. 10.) ;
          ctx##strokeRect (x +. 30.) (model.h -. pad)  (dx -. 60.) (-. head *. 10.) ;
          draw_bar dx (x +. dx) rest ; 
          ()
      in
      ctx##.strokeStyle := Js.string "#1f70a3" ;
      ctx##.fillStyle := Js.string "#6cb7e6" ;
      draw_bar dx pad model.data ;
      ()
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let data = [ 16.; 68.; 20.; 30.; 54. ] in
  let model = { w; h; data } in
  let chart = new bar_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init