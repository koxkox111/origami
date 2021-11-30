(*  Autor: Pawel Dec
    Reviewer: Karol Wasowski
*)

type point = float * float

type kartka = point -> int

let eps = 1e-9 (* zalozenie - brak  skrajnych przypadkow *)

let sqr x = x *. x

let abs x = if x > 0. then x else (-1. *. x)

let prosta (x1,y1) (x2,y2) =  (* zwraca (A,B) prostej y = Ax+B  przechodzcej przez dwa punkty *)
    (((y2 -. y1) /. (x2 -. x1)), y1 -.  ((y2 -. y1) /. (x2 -. x1)) *. x1)

let prostopadla (a,b) (x,y) = (* zwraca (A,B) prostej prostopadlej do (x,y) przechodzacej przez (a,b) *)
    ((-1.) /. x , (b -.  a *. ((-1.) /. x)))

let przeciecie (x1,y1) (x2,y2) = (* zwraca przeciecie dwoch prostych *)
    ((y2 -. y1) /. (x1 -. x2), x1 *. ((y2 -. y1) /. (x1 -. x2)) +. y1)

let odbicie (a1,b1) (a2,b2) = (* symetria punkty (a1,b1) wzgledem punktu (a2,b2) *)
    (2. *. (a2 -. a1) +. a1, 2. *. (b2 -. b1) +. b1)

let symetria (a,b) (x1,y1) (x2,y2) = (* odbicie punktu wzgledem prostej *)
    if x1 = x2 then (a +. 2. *. (x1 -.a),b)
    else if y1 = y2 then (a, 2. *. y1  -. b)
    else let (c,d) = prosta (x1,y1)(x2,y2) in       (* prosta  A, os symetri *)      
        let (e,f) = prostopadla (a,b) (c,d) in          (* prosta prostobala do A, przechodzi przez (a,b) *)
        let (g,h) = przeciecie (c,d) (e,f) in       (* przeciecie tych dwoch prostych *)
        odbicie (a,b) (g,h) 


let iloczyn_wek (x,y)(a,b) =
    x *. b -. y *. a
   
let status (a,b) (x1,y1) (x2,y2) =     (* -1 punkt jest na lewo, 1 na prawo, 0 na prostej *)
    let ret = iloczyn_wek (x2-.x1,y2-.y1) (a-.x1,b-.y1) in
    if abs(ret) <= eps then 0 else if(ret < 0.) then -1  else 1

let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka =
fun (px, py) ->
	if x1 -. px <= eps && px -. x2 <= eps && y1 -. py <= eps && py -. y2 <= eps then 1
	else 0  

let kolko ((x,y):point) (r:float) = function (a,b) -> 
    if eps >= sqr(x -. a) +. sqr(y -. b)  -. sqr(r) then 1 else 0

let zloz (x1,y1) (x2,y2) (k:kartka) (a,b) =
    let d = status (a,b) (x1,y1) (x2,y2) and s = symetria (a,b) (x1,y1)(x2,y2) in
        if d = -1 then
            0
        else if d = 1 then
            k (a,b) + k (s)
        else
            k (a,b)
            
let skladaj l k = 
    List.fold_left (fun a  (x,y) -> zloz x y a) k l