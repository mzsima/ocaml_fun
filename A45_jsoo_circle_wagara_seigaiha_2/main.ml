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
      let r = 20.0 in
      let o = {x=model.w/.2. -. r *. 20.;y=model.h/.2. -. r *. 20.} in
      let strokeColor = Js.string "#004C71" in
      ctx##.fillStyle := Js.string "rgba(255, 255, 255, 1)" ;
      ctx##.strokeStyle := strokeColor ;
      ctx##.lineWidth := 4.5;
      ctx##fillRect 0. 0. model.w model.h ;
      ctx##.font := Js.string "bold 24px roboto" ;

      let draw_circle i j =
          let x, y = ref (float i *. r *. 4. +. o.x), (float j *. r +. o.y)
          in
          ctx##beginPath ;
          if j mod 2 == 0 then 
            x := (!x -. 2. *. r) ;
          ctx##arc !x y (2. *. r) 0. (pi *. 2.0) (Js.bool false) ;
          ctx##fill ;

          let rec draw_inner_circle a =
            ctx##beginPath ;
            ctx##save ;
            let r0 = (-.pi *. 0.3) in 
            let r1 = r0 +. pi *. (2. -. 0.2) in
            ctx##arc !x y (a *. r) r0 r1 (Js.bool false) ;
            ctx##stroke ;
            if a < 0.5 then ()
            else draw_inner_circle (a -. 0.32)
          in
          draw_inner_circle 1.7 ;
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