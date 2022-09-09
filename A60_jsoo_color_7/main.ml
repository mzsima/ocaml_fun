open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

class colorbar =
  object

    method draw (ctx : canvasCtx)  = 
      ctx##translate 40. 40. ;
      let r = 24.0 in
      for i = 0 to 19 do
        for j = 0 to 10 do
          let l = 0.5 +. (sin ((float j) *. pi /. 5.)) *. 0.2 in
          let strokeColor = Color.of_hsl ((float i) *. 18.) 1.0 l in
          let x, y = r *. (float i), 2. *. r *. ((float j) +. if i mod 2 == 0 then 0. else 0.5) in
          ctx##beginPath ;
          ctx##moveTo (x -. r *. 0.4) (y -. r *. 0.5);
          ctx##lineTo (x +. r *. 0.4) (y +. r *. 0.5) ;
          ctx##.lineWidth := 28.;
          ctx##.strokeStyle := Js.string (Color.to_css_hsla strokeColor) ;
          ctx##stroke ;
        done
      done ;
      for i = 0 to 19 do
        for j = 0 to 10 do
          let x, y = r *. (float i), 2. *. r *. ((float j) +. if i mod 2 == 0 then 0. else 0.5) in
          ctx##beginPath ;
          ctx##arc x y r 0. (pi *. 2.0) (Js.bool false) ;
          ctx##.lineWidth := 2.;
          ctx##.strokeStyle := Js.string ("lightgray") ;
          ctx##stroke ;
        done
      done
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let colorbar = new colorbar  in
  colorbar#draw c ;
  ()

let () = Js.export "init" init