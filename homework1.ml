(* 

HOMEWORK 1

Name: Anne LoVerso

Email: anne.loverso@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

let rec gcd (a,b) = 
   if a = 0 then b
   else if b = 0 then a
   else if a > b then
      gcd(a-b,b)
   else gcd(a,b-a)

let is_coprime (a,b) = 
   if gcd(a,b) = 1 then true
   else false

let rec euler_helper (n, orign) =
   if n = 1 then 1
   else if is_coprime(n, orign) then euler_helper(n-1, orign) + 1
   else euler_helper(n-1, orign)

let euler (n) = 
   euler_helper(n, n)

let rec coprimes_helper (n, orign) = 
   if n = 1 then [1]
   else if is_coprime(n, orign) then coprimes_helper(n-1, orign) @ [n]
   else coprimes_helper(n-1, orign)


let coprimes (n) = 
   coprimes_helper(n,n)


(* Question 2 *)

let rec insert_at_end (lst,item) =
   match lst with [] -> [item]
   | first::rest -> first :: insert_at_end (rest, item)

let rec append (xs,ys) = 
   if xs = [] then ys
   else if ys = [] then xs
   else
      match ys with first::rest -> append(insert_at_end(xs, first), rest)

let rec flatten_helper(xss, res) =
   if xss = [[]] then res
   else if xss = [] then res
   else match xss with first::rest -> flatten_helper(rest, append(res,first))

let rec flatten (xss) = 
   flatten_helper(xss,[])

let rec nth (n,xs) = 
   if xs = [] then failwith "out of bounds"
   else match xs with first::rest -> 
      if rest = [] && n>0 then failwith "out of bounds"
      else if n=0 then first
      else nth(n-1, rest)

let rec last (xs) = 
   if xs = [] then failwith "empty list"
   else match xs with first::rest -> if rest = [] then first else last(rest)

let rec separate_helper (xs, res1, res2) = 
   if xs = [] then (res1, res2)
   else match xs with first::rest ->
      match first with (x,y) ->
         separate_helper(rest, insert_at_end(res1, x), insert_at_end(res2, y))

let separate (xs) = 
   separate_helper(xs, [], [])
   

(* Question 3 *)

let rec setIn (e,xs) = 
   if xs = [] then false
   else match xs with first::rest -> 
      if rest = [] && e <> first then false
      else if e = first then true
      else setIn(e, rest)

let rec setSub (xs,ys) = 
   if xs = [] then true
   else match xs with first::rest -> setIn(first, ys) && setSub(rest,ys)

let setEqual (xs,ys) = 
   setSub(xs, ys) && setSub(ys, xs)

let rec setUnion_helper(xs,ys,res) =
   if xs = [] then
      if ys = [] then res
      else match ys with first::rest ->
         if not(setIn(first,res)) then setUnion_helper(xs, rest, insert_at_end(res, first))
         else setUnion_helper(xs, rest, res)
   else match xs with first::rest ->
      if not(setIn(first,res)) then setUnion_helper(rest, ys, insert_at_end(res, first))
      else setUnion_helper(rest, ys, res)

let setUnion (xs,ys) = 
   setUnion_helper(xs,ys,[])

let rec setInter_helper (xs,ys,res) =
   if xs = [] then
      if ys = [] then res
      else match ys with first::rest ->
         if not(setIn(first,res)) && setIn(first,xs) then setInter_helper(xs, rest, insert_at_end(res, first))
         else setInter_helper(xs, rest, res)
   else match xs with first::rest ->
      if not(setIn(first,res)) && setIn(first,ys) then setInter_helper(rest, ys, insert_at_end(res, first))
      else setInter_helper(rest, ys, res)

let setInter (xs,ys) = 
   setInter_helper(xs,ys,[])

let rec length_helper (xs, n) =
   if xs = [] then n
   else match xs with first::rest -> length_helper(rest, n+1) 

let length (xs) =
   length_helper(xs, 0)

let setSize (xs) = 
   length(setUnion(xs,xs))
