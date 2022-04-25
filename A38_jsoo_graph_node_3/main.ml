open Js_of_ocaml
open Graphics_js

module Html = Dom_html
let pi = acos (-1.)

type point = {x : float; y : float}
type edge = {from_n : int; to_n : int}
type model = { w: float; h: float; data: point array; edge: edge list; } 
type canvasElement = Html.canvasElement Js.t  
type canvasCtx = Html.canvasRenderingContext2D Js.t

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
      let r = 12.0 in
      let bgColor = Js.string "#2b3d5b" in

      ctx##beginPath ;
      ctx##.lineWidth := 2.;
      ctx##.fillStyle := bgColor ;
      ctx##.strokeStyle := Js.string "#101010" ;
      ctx##fillRect 0. 0. model.w model.h ;

      let stroke_curve_up p0 p1 = 
        ctx##moveTo p0.x p0.y ;
        ctx##quadraticCurveTo o.x o.y p1.x p1.y ;
        ctx##stroke 
      in

      let draw_child ctx data =
        let f elem = 
          ctx##beginPath ;
          ctx##arc (o.x +. elem.x) (o.y +. elem.y) r 0. (pi *. 2.) (Js.bool false);
          ctx##fill ;
        in 
          Array.iter f data
      in
      let stroke_child ctx data edge =
        let f elem = 
          let fr_n = data.(elem.from_n) in
          let to_n = data.(elem.to_n) in
          ctx##beginPath ;
          stroke_curve_up  {x=(o.x +. fr_n.x); y=(o.y +. fr_n.y)} {x=(o.x +. to_n.x); y=(o.y +. to_n.y)}
        in 
          List.iter f edge
      in
      ctx##.fillStyle := Js.string "#fefefe" ;
      ctx##.strokeStyle := Js.string "#fefefe" ;
      draw_child ctx model.data;
      stroke_child ctx model.data model.edge;
      ()
end

let randomData = 
  let r = 200. in
  Array.init 30 (fun i -> {x= r *. (sin (pi *. (float i) /. 15.)); y= r *. (cos (pi *. (float i) /. 15.))})

let randomEdge = 
  List.init 100 (fun _ -> {from_n=(Random.int 30);to_n=(Random.int 30)})


let init canvas =
  print_endline "initializing";
  let () = open_canvas canvas in
  let c = canvas##getContext Html._2d_ in
  let w, h = (float canvas##.width), (float canvas##.height) in
  let data = randomData in
  let edge = randomEdge in
  let model = { w; h; data; edge } in
  let chart = new pie_chart model in
  chart#draw c ;
  ()

let () = Js.export "init" init