(* definition of `node'
   marked `Lazy.t' because we'd create self-referencing nodes
   marked `mutable' because Schorr-Waite algorithm would modify the pointer fields
*)
type 'a node = {key : 'a; mutable left : 'a node Lazy.t; mutable right : 'a node Lazy.t}

let nil =
  let rec aux = lazy ({key = 0; left = lazy (Lazy.force aux); right = lazy (Lazy.force aux)}) in
  Lazy.force aux
    
let example =
  (* create a complete binary tree with 8 nodes *)
  let a = Array.make 9 nil in
  let rec go i =
    a.(i) <- {key = i; left = if i*2 < 9 then lazy a.(i*2) else lazy nil; right = if i*2+1 < 9 then lazy a.(i*2+1) else lazy nil};
    if i > 1 then go (i-1) in
  go (9-1);
  
  (* make some circles to demonstrate that this algorithm deal with non-trees *)
  a.(8).left <- lazy a.(3);
  a.(8).right <- lazy a.(2);
  a.(5).right <- lazy a.(1);
  a

let schorr_waite root =
  if root != nil then
    let c = Array.make 9 0 in
    c.(nil.key) <- 3; (* mark nil as been visited three times *)
    let dummy = {key = -1; left = lazy root; right = lazy nil} in (* dummy node *)
    let p = ref root in
    let q = ref dummy in
    while !p != dummy do
      print_endline (string_of_int !p.key);
      c.(!p.key) <- c.(!p.key) + 1; (* number of times been visited *)
      if c.(!p.key) = 3 || c.((Lazy.force !p.left).key) = 0 then begin (* swing four pointers *)
	let r = Lazy.force !p.left in
	!p.left <- !p.right;
	!p.right <- Lazy.lazy_from_val !q;
	q := !p;
	p := r
      end else begin (* swing three pointers *)
	let r = Lazy.force !p.left in
	!p.left <- !p.right;
	!p.right <- Lazy.lazy_from_val !q;
	q := r
      end
    done;;

schorr_waite example.(1);;
