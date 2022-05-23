open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type model = { w: float; h: float; count: int; } 
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class pie_chart model =
  object

    method draw (ctx : canvasCtx)  = 
      let r = 18.0 in
      let o = {x=model.w/.2. -. r *. 20.;y=model.h/.2. -. r *. 20.} in
      let bgColor = Js.string "#2b3d5b" in

      ctx##.lineWidth := 2.;
      ctx##.fillStyle := bgColor ;
      ctx##fillRect 0. 0. model.w model.h ;
      ctx##.font := Js.string "bold 24px roboto" ;

      let draw_triangle i j s =
          let x, y = (float i *. r *. 2. +. o.x), (float j *. r *. 2. +. o.y)
          in
          ctx##beginPath ;
          ctx##moveTo x y ;
          ctx##lineTo (x +. (float s *. 2. +. 15.)) y ;
          ctx##lineTo x (y +. (float s *. 2. +. 15.)) ;
          ctx##closePath ;
          ctx##.fillStyle := Js.string "#000000" ;
          ctx##fill ;
      in
      for i = 0 to model.count - 1 do
        for j = 0 to model.count - 1 do
          draw_triangle i j ((i + j) mod 20)
        done
      done
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let model = { w; h; count=20; } in
  let chart = new pie_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init