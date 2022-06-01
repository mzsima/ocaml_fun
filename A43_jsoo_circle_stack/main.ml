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

      let draw_circle i j =
          let x, y = (float i *. r *. 4. +. o.x), (float j *. r *. 2. +. o.y)
          in
          ctx##beginPath ;
          if j mod 2 == 0 then
            ctx##arc x y (2. *. r) 0. (pi *. 2.) (Js.bool false) 
          else 
            ctx##arc (x -. 2. *. r) y (2. *. r) 0. (pi *. 2.) (Js.bool false) ;
          ctx##.strokeStyle := Js.string "rgba(255, 255, 255, 0.2)" ;
          ctx##fill ;
          ctx##stroke ;
      in
      for j = 0 to model.count - 1 do
        for i = 0 to model.count - 1 do
          draw_circle i j
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