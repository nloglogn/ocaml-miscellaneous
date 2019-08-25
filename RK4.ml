let param = 1.0
let init = (0.0,0.1)
let t_max = 100.0
let h = 0.001

let func (x,y) = (y, param *. (1.0 -. x *. x) *. y -. x) (* van der Pol 振動子 *)

let rk4 f init =
  let n_max = int_of_float (t_max /. h) in
  let rec iter f init sol k n =
    if k = n then sol
    else
      let k1 = f init in
      let k2 = f ((fst init) +. (fst k1) *. h /. 2.0, (snd init) +. (snd k1) *. h /. 2.0) in
      let k3 = f ((fst init) +. (fst k2) *. h /. 2.0, (snd init) +. (snd k2) *. h /. 2.0) in
      let k4 = f ((fst init) +. (fst k3) *. h, (snd init) +. (snd k3) *. h) in
      let next = ((fst init) +. ((fst k1) +. 2.0 *. (fst k2) +. 2.0 *. (fst k3) +. (fst k4)) *. h /. 6.0,
                  (snd init) +. ((snd k1) +. 2.0 *. (snd k2) +. 2.0 *. (snd k3) +. (snd k4)) *. h /. 6.0) in
                  (*let () = print_float (fst init)
                in*) iter f next (next::sol) (k+1) n
  in List.rev(iter f init [init] 0 n_max)

let filename = "output.dat"
let out = open_out_gen [Open_wronly; Open_append; Open_text] 0o666 filename
let rec output data filename =
  match data with
      [] -> close_out out
   | (x,y) :: rest -> 
        begin 
          output_string out ((string_of_float x) ^ " " ^ (string_of_float y) ^ "\n");
          output rest filename
        end
let sol = rk4 func init
let () = output sol filename