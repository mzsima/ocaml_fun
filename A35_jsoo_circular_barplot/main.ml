open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type model = { w: float; h: float; data: float list } 

type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t
type point = {x : float; y : float}

let generateColor () = 
  Random.self_init () ;
  let hex = "6789ABCDEF" in
  let rec color cnt s = match cnt with
    | 0 -> s
    | _ -> color (cnt - 1) (s ^ (String.make 1 hex.[(Random.int 9)])) in
  color 6 "#"

let rec rangef a b =
  if a > b then []
  else a::rangef (a +. 1.) b

class pie_chart model =
  object

    method draw (ctx : canvasCtx)  = 
      let o = {x=model.w/.2.;y=model.h/.2.} in
      let r = 120.0 in
      let bgColor = Js.string "#2b3d5b" in

      ctx##beginPath ;
      ctx##.lineWidth := 5.;
      ctx##.fillStyle := bgColor ;
      ctx##.strokeStyle := bgColor ;
      ctx##fillRect 0. 0. model.w model.h ;

      let rec draw_pie prevAngle data total = match data with 
        | [] -> ()
        | head :: rest ->  
          let color = generateColor () in
          let angle =  (pi *. 2. /. total) +. prevAngle in
          ctx##.fillStyle := Js.string color;
          ctx##beginPath ;
          ctx##moveTo o.x o.y ;
          ctx##arc o.x o.y (r +. head *. 2.4) prevAngle angle (Js.bool false) ;
          ctx##lineTo o.x o.y ;
          ctx##fill ;
          ctx##stroke ;
          draw_pie angle rest total ;
          ()
      in
      draw_pie 0. model.data (float (List.length model.data));
      ctx##.fillStyle := bgColor ;
      ctx##beginPath ;
      ctx##arc o.x o.y r 0. (pi *. 2.) (Js.bool false);
      ctx##fill ;
      ctx##stroke ;
      ()
end

let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let data = List.mapi (fun i e -> e +. 5. *. (sin (pi /. 3. *. (float i)))) (rangef 30. 80.) in
  let model = { w; h; data } in
  let chart = new pie_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init