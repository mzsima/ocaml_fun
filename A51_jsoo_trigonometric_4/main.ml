open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type model = { w: float; h: float; count: int; } 
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class circle model =
  object

    method draw (ctx : canvasCtx)  = 
      let o = {x=model.w/.2. ;y=model.h/.2. } in
      let strokeColor = Js.string "#1e1e1e" in
      ctx##.strokeStyle := strokeColor ;
      ctx##.lineWidth := 2.;
      ctx##translate o.x o.y ;
      let draw_circle (i: int) =
          let angle = (float i) *. pi /. 80. in
          let r1 = 280.0 *. ((sin (angle *. 6. +. pi /. 6.)) *. 0.2 +. 0.8) in
          let r2 = 15.0 *. (sin (angle *. 6.)) +. 12. in
          let x = r1 *. (cos angle) in
          let y = r1 *. (sin angle) in
          ctx##beginPath ;
          ctx##arc x y (max 1. r2) 0. (pi *. 2.0) (Js.bool false) ;
          ctx##stroke ;
      in
      for i = 0 to model.count - 1 do
          draw_circle (i)
      done
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let model = { w; h; count=160; } in
  let chart = new circle model in
  chart#draw c ;
  ()

let () = Js.export "init" init