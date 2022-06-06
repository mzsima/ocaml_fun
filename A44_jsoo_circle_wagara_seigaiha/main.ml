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
      let bgColor = Js.string "#004C71" in
      ctx##.fillStyle := bgColor ;
      ctx##fillRect 0. 0. model.w model.h ;
      ctx##.font := Js.string "bold 24px roboto" ;

      let draw_circle i j =
          let x, y = ref (float i *. r *. 4. +. o.x), (float j *. r +. o.y)
          in
          ctx##beginPath ;
          if j mod 2 == 0 then 
            x := (!x -. 2. *. r) ;
          ctx##.lineWidth := 2.;
          ctx##.strokeStyle := Js.string "rgba(255, 255, 255, 1)" ;
          ctx##arc !x y (2.1 *. r) 0. (pi *. 2.) (Js.bool false) ;
          let rec draw_inner_circle a =
            ctx##arc !x y (a *. r) 0. (pi *. 2.) (Js.bool false) ; 
            if a < 0.8 then ()
            else draw_inner_circle (a -. 0.4)
          in
          draw_inner_circle 2.0 ;
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