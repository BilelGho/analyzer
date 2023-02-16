open GobConfig
open GoblintCil

module GU = Goblintutil
module M = Messages
module BI = IntOps.BigIntOps

module IntervalSetFunctor(Ints_t : IntOps.IntOps): IntDomain.SOverFlow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) list =
struct

  module Interval = IntDomain.IntervalFunctor(Ints_t)

  let name () = "IntervalSets"

  type int_t = Ints_t.t

  (* 
    Each domain's element is guaranteed to be in canonical form. That is, each interval contained
    inside the set does not overlap with each other and they are not adjecent. 
  *)
  type t = (Ints_t.t * Ints_t.t) list [@@deriving eq, hash, ord]

  let range ik = BatTuple.Tuple2.mapn Ints_t.of_bigint (IntDomain.Size.range ik)

  let top () = failwith @@ "top () not implemented for " ^ (name ())

  let top_of ik = [range ik]

  let bot () = []

  let bot_of ik = bot () 

  let show (x: t) =
    let show_interval i = Printf.sprintf "[%s, %s]" (Ints_t.to_string (fst i)) (Ints_t.to_string (snd i)) in
    List.fold_left (fun acc i -> (show_interval i) :: acc) [] x |> List.rev |> String.concat ", " |> Printf.sprintf "[%s]"

  (* New type definition for the sweeping line algorithm used for implementiong join/meet functions. *)
  type 'a event = Enter of 'a | Exit of 'a

  let unbox_event = function Enter x -> x | Exit x -> x

  let _show_event = function 
    | Enter x -> Printf.sprintf "Enter %s" (Ints_t.to_string x) 
    | Exit x -> Printf.sprintf "Exit %s" (Ints_t.to_string x)

  let cmp_events x y = 
    let res = Ints_t.compare (unbox_event x) (unbox_event y) in
    if res <> 0 then res
    else
      begin
        match (x, y) with
        | (Enter _, Exit _) -> -1
        | (Exit _, Enter _) -> 1
        | (_, _) -> 0
      end

  let interval_set_to_events (xs: t) = 
    List.concat_map (fun (a, b) -> [Enter a; Exit b]) xs

  let two_interval_sets_to_events (xs: t) (ys: t) =
    let xs = interval_set_to_events xs in
    let ys = interval_set_to_events ys in
    List.merge cmp_events xs ys

  (* Using the sweeping line algorithm, combined_event_list returns a new event list representing the intervals in which at least n intervals in xs overlap 
     This function is used for both join and meet operations with different parameter n: 1 for join, 2 for meet *)   
  let combined_event_list lattice_op (xs: int_t event list)  =
    let l = match lattice_op with `Join -> 1 | `Meet -> 2 in
    let aux (interval_count, acc) = function
      | Enter x -> (interval_count + 1, if (interval_count + 1) >= l && interval_count < l then (Enter x)::acc else acc)
      | Exit x -> (interval_count - 1, if interval_count >= l && (interval_count - 1) < l then (Exit x)::acc else acc) 
    in
    List.fold_left aux (0, []) xs |> snd |> List.rev

  let rec events_to_intervals = function
    | [] -> []
    | (Enter x)::(Exit y)::xs  -> (x, y)::(events_to_intervals xs)
    | _ -> failwith "Invalid events list"

  let remove_empty_gaps (xs: t) = 
    let aux acc (l, r) = match acc with
      | ((a, b)::acc') when Ints_t.compare (Ints_t.add b (Ints_t.one)) l >= 0 -> (a, r)::acc'
      | _ -> (l, r)::acc
    in 
    List.fold_left aux [] xs |> List.rev

  let canonize (xs: t) = 
    interval_set_to_events xs |>
    List.sort cmp_events |>
    combined_event_list `Join |> 
    events_to_intervals |>
    remove_empty_gaps

  let unary_op (x: t) op = match x with 
    | [] -> []
    | _ -> canonize @@ List.concat_map op x

  let binary_op (x: t) (y: t) op : t = match x, y with
    | [], _ -> []
    | _, [] -> []
    | _, _ -> canonize @@ List.concat_map op (BatList.cartesian_product x y)

  include IntDomain.Std (struct type nonrec t = t let name = name let top_of = top_of let bot_of = bot_of let show = show let equal = equal end)

  let minimal = function 
    | [] -> None 
    | (x, _)::_ -> Some x

  let maximal = function
    | [] -> None
    | xs -> let max = BatList.last xs |> snd in Some max

  let equal_to_interval i (a, b) = 
    if a = b && b = i then 
      `Eq 
    else if Ints_t.compare a i <= 0 && Ints_t.compare i b <=0 then 
      `Top 
    else 
      `Neq

  let equal_to i xs = match List.map (equal_to_interval i) xs with
    | [] -> failwith "unsupported: equal_to with bottom"
    | [`Eq] ->  `Eq 
    | ys -> if List.for_all (fun x -> x = `Neq) ys  then `Neq else `Top  

  let norm_interval ?(suppress_ovwarn=false) ?(cast=false) ik (x,y) : t*bool*bool*bool = 
    if Ints_t.compare x y > 0 then ([],false,false,cast)  
    else begin
      let (min_ik, max_ik) = range ik in
      let underflow = Ints_t.compare min_ik x > 0 in
      let overflow = Ints_t.compare max_ik y < 0 in
      if underflow || overflow then
        begin
          if IntDomain.should_wrap ik then (* could add [|| cast], but that's GCC implementation-defined behavior: https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html#Integers-implementation *)
            (* We can only soundly wrap if at most one overflow occurred, otherwise the minimal and maximal values of the interval *)
            (* on Z will not safely contain the minimal and maximal elements after the cast *)
            let diff = Ints_t.abs (Ints_t.sub max_ik min_ik) in
            let resdiff = Ints_t.abs (Ints_t.sub y x) in
            if Ints_t.compare resdiff diff > 0 then
              ([range ik], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
            else
              let l = Ints_t.of_bigint @@ IntDomain.Size.cast ik (Ints_t.to_bigint x) in
              let u = Ints_t.of_bigint @@ IntDomain.Size.cast ik (Ints_t.to_bigint y) in
              if Ints_t.compare l u <= 0 then
                ([(l, u)], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
              else
                (* Interval that wraps around (begins to the right of its end). We CAN represent such intervals *)
                ([(min_ik, u); (l, max_ik)], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
          else if not cast && IntDomain.should_ignore_overflow ik then
            let tl, tu = range ik in
            ([Ints_t.max tl x, Ints_t.min tu y], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
          else
            ([range ik], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
        end
      else 
        ([(x,y)], underflow && not suppress_ovwarn, overflow && not suppress_ovwarn, cast)
    end


  let norm_intvs ?(suppress_ovwarn=false) ?(cast=false) (ik:ikind) (xs: t) : t*bool*bool*bool = 
    let res = List.map (norm_interval ~suppress_ovwarn ~cast ik) xs in
    let intvs = List.concat_map IntDomain.unlift res in
    let underflow = List.exists (fun (_,underflow,_,_) -> underflow) res in
    let overflow = List.exists (fun (_,_,overflow,_) -> overflow) res
    in (intvs,underflow,overflow,cast)

  let binary_op_with_norm  (ik:ikind) (x: t) (y: t) op : t*bool*bool*bool = match x, y with
    | [], _ -> ([],false,false,false)
    | _, [] -> ([],false,false,false)
    | _, _ -> let (res,underflow,overflow,cast) = norm_intvs ik @@ List.concat_map op (BatList.cartesian_product x y) 
      in (canonize res, underflow,overflow,cast)

  let binary_op_with_ovc (x: t) (y: t) op : t*bool*bool*bool = match x, y with
    | [], _ -> ([],false,false,false)
    | _, [] -> ([],false,false,false)
    | _, _ ->
      let res = List.map op (BatList.cartesian_product x y) in
      let intvs = List.concat_map IntDomain.unlift res in
      let underflow = List.exists (fun (_,underflow,_,_) -> underflow) res in
      let overflow = List.exists (fun (_,_,overflow,_) -> overflow) res
      in (canonize intvs, underflow,overflow,false)

  let unary_op_with_norm (ik:ikind) (x: t) op = match x with 
    | [] -> ([],false,false,false)
    | _ -> let (res,underflow,overflow,cast) = norm_intvs ik @@ List.concat_map op x in (canonize res, underflow,overflow,cast)

  let rec leq (xs: t) (ys: t) =
    let leq_interval (al, au) (bl, bu) = Ints_t.compare al bl >= 0 && Ints_t.compare au bu <= 0 in
    match xs, ys with
    | [], _ -> true
    | _, [] -> false
    | (xl,xr)::xs', (yl,yr)::ys' -> if leq_interval (xl,xr) (yl,yr) then
        leq xs' ys else if Ints_t.compare xr yl < 0 then false else leq xs ys'

  let join ik (x: t) (y: t): t = 
    two_interval_sets_to_events x y |> 
    combined_event_list `Join |>
    events_to_intervals |>
    remove_empty_gaps

  let meet ik (x: t) (y: t): t = 
    two_interval_sets_to_events x y |> 
    combined_event_list  `Meet |> 
    events_to_intervals 

  let to_int = function 
    | [(x, y)] when Ints_t.compare x y = 0 -> Some x 
    | _ -> None

  let zero = [(Ints_t.zero, Ints_t.zero)]
  let one = [(Ints_t.one, Ints_t.one)]

  let top_bool = [(Ints_t.zero, Ints_t.one)]

  let not_bool (x:t) = 
    let is_false x = x = zero in
    let is_true x = x = one in
    if is_true x then zero else if is_false x then one else top_bool 

  let to_bool = function  
    | [(l,u)] when Ints_t.compare l Ints_t.zero = 0 && Ints_t.compare u Ints_t.zero = 0 -> Some false
    | x -> if leq zero x then None else Some true

  let of_bool _ = function true -> one | false -> zero 

  let of_interval ?(suppress_ovwarn=false) ik (x,y) =  norm_interval  ~suppress_ovwarn ~cast:false ik (x,y)

  let of_int ik (x: int_t) = of_interval ik (x, x)

  let lt ik x y = 
    match x, y with 
    | [], [] -> bot_of ik
    | [], _ | _, [] -> raise (IntDomain.ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ ->
      let (max_x, min_y) = (maximal x |> Option.get , minimal y |> Option.get) in
      let (min_x, max_y) = (minimal x |> Option.get , maximal y |> Option.get) in
      if max_x < min_y then 
        of_bool ik true
      else
      if min_x >= max_y then of_bool ik false else top_bool

  let le ik x y =
    match x, y with 
    | [], [] -> bot_of ik
    | [], _ | _, [] -> raise (IntDomain.ArithmeticOnIntegerBot (Printf.sprintf "%s op %s" (show x) (show y)))
    | _, _ ->
      let (max_x, min_y) = (maximal x |> Option.get , minimal y |> Option.get) in
      let (min_x, max_y) = (minimal x |> Option.get , maximal y |> Option.get) in
      if max_x <= min_y then 
        of_bool ik true
      else
      if min_x > max_y then of_bool ik false else top_bool

  let gt ik x y = not_bool @@ le ik x y

  let ge ik x y = not_bool @@ lt ik x y

  let eq ik x y = match x, y with
    | (a, b)::[], (c, d)::[] when (Ints_t.compare a b) == 0 && (Ints_t.compare c d) == 0 && (Ints_t.compare a c) == 0 -> 
      one
    | _ -> 
      if is_bot (meet ik x y) then 
        zero 
      else 
        top_bool

  let ne ik x y = not_bool @@ eq ik x y
  let interval_to_int i = Interval.to_int (Some i)
  let interval_to_bool i = Interval.to_bool (Some i)

  let log f ik (i1, i2) = 
    match (interval_to_bool i1, interval_to_bool i2) with
    | Some x, Some y -> of_bool ik (f x y)
    | _ -> top_of ik


  let bit f ik (i1, i2) =
    match (interval_to_int i1), (interval_to_int i2) with
    | Some x, Some y -> (try of_int ik (f x y) |> IntDomain.unlift with Division_by_zero -> top_of ik)
    | _ -> top_of ik


  let bitcomp f ik (i1, i2) = 
    match (interval_to_int i1, interval_to_int i2) with 
    | Some x, Some y -> (try of_int ik (f x y) with Division_by_zero | Invalid_argument _ -> (top_of ik,false,false,false))
    | _, _ -> (top_of ik,true,true,false)

  let bitand ik x y = 
    let interval_bitand = bit Ints_t.bitand ik in
    binary_op x y interval_bitand

  let bitor ik x y = 
    let interval_bitor = bit Ints_t.bitor ik in
    binary_op x y interval_bitor

  let bitxor ik x y = 
    let interval_bitxor = bit Ints_t.bitxor ik in
    binary_op x y interval_bitxor

  let bitnot ik x = 
    let bit1 f ik i1 =
      match interval_to_int i1 with
      | Some x -> of_int ik (f x) |> IntDomain.unlift
      | _ -> top_of ik
    in
    let interval_bitnot = bit1 Ints_t.bitnot ik in
    unary_op x interval_bitnot

  let shift_left ik x y = 
    let interval_shiftleft = bitcomp (fun x y -> Ints_t.shift_left x (Ints_t.to_int y)) ik in
    binary_op_with_ovc x y interval_shiftleft

  let shift_right ik x y = 
    let interval_shiftright = bitcomp (fun x y -> Ints_t.shift_right x (Ints_t.to_int y)) ik in
    binary_op_with_ovc x y interval_shiftright

  let lognot ik x = 
    let log1 f ik i1 = 
      match interval_to_bool i1 with
      | Some x -> of_bool ik (f x)
      | _ -> top_of ik
    in
    let interval_lognot = log1 not ik in
    unary_op x interval_lognot

  let logand ik x y = 
    let interval_logand = log (&&) ik in
    binary_op x y interval_logand

  let logor ik x y = 
    let interval_logor = log (||) ik in
    binary_op x y interval_logor

  let add ?no_ov ik x y =
    let interval_add ((x1, x2), (y1, y2)) = [(Ints_t.add x1 y1, Ints_t.add x2 y2)] in
    binary_op_with_norm ik x y interval_add

  let neg ?no_ov ik x = 
    let neg_interval ((x, y)) = [(Ints_t.neg y, Ints_t.neg x)] in
    unary_op_with_norm ik x neg_interval

  let sub ?no_ov ik x y = 
    let interval_sub ((x1, x2), (y1, y2)) = [(Ints_t.sub x1 y2, Ints_t.sub x2 y1)] in
    binary_op_with_norm  ik x y interval_sub

  let mul ?no_ov (ik: ikind) (x: t) (y: t) = 
    let interval_mul ((x1, x2), (y1, y2)) = 
      let x1y1 = (Ints_t.mul x1 y1) in
      let x1y2 = (Ints_t.mul x1 y2) in
      let x2y1 = (Ints_t.mul x2 y1) in
      let x2y2 = (Ints_t.mul x2 y2) in
      [((Ints_t.min (Ints_t.min x1y1 x1y2) (Ints_t.min x2y1 x2y2)), (Ints_t.max (Ints_t.max x1y1 x1y2) (Ints_t.max x2y1 x2y2)))]
    in
    binary_op_with_norm ik x y interval_mul

  let div ?no_ov ik x y = 
    let rec interval_div ((x1, x2), (y1, y2)) = begin
      let is_zero v = Ints_t.compare v Ints_t.zero = 0 in
      match y1, y2 with
      | l, u when is_zero l && is_zero u -> top_of ik (* TODO warn about undefined behavior *)
      | l, _ when is_zero l              -> interval_div ((x1,x2), (Ints_t.one,y2))
      | _, u when is_zero u              -> interval_div ((x1,x2), (y1, Ints_t.(neg one)))
      | _ when leq (of_int ik (Ints_t.zero) |> IntDomain.unlift) ([(y1,y2)]) -> top_of ik
      | _ ->
        let x1y1n = (Ints_t.div x1 y1) in 
        let x1y2n = (Ints_t.div x1 y2) in
        let x2y1n = (Ints_t.div x2 y1) in 
        let x2y2n = (Ints_t.div x2 y2) in
        let x1y1p = (Ints_t.div x1 y1) in 
        let x1y2p = (Ints_t.div x1 y2) in
        let x2y1p = (Ints_t.div x2 y1) in 
        let x2y2p = (Ints_t.div x2 y2) in
        [((Ints_t.min (Ints_t.min x1y1n x1y2n) (Ints_t.min x2y1n x2y2n)),
          (Ints_t.max (Ints_t.max x1y1p x1y2p) (Ints_t.max x2y1p x2y2p)))]
    end
    in binary_op_with_norm ik x y interval_div

  let rem ik x y = 
    let interval_rem (x, y) = 
      if Interval.is_top_of ik (Some x) && Interval.is_top_of ik (Some y) then
        top_of ik
      else
        let (xl, xu) = x in let (yl, yu) = y in
        let pos x = if Ints_t.compare x Ints_t.zero < 0 then Ints_t.neg x else x in
        let b = Ints_t.sub (Ints_t.max (pos yl) (pos yu)) Ints_t.one in
        let range = if Ints_t.compare xl Ints_t.zero>= 0 then (Ints_t.zero, Ints_t.min xu b) else (Ints_t.max xl (Ints_t.neg b), Ints_t.min (Ints_t.max (pos xl) (pos xu)) b) in
        meet ik (bit Ints_t.rem ik (x, y)) [range]
    in
    binary_op x y interval_rem

  let cast_to ?torg ?no_ov ik x = norm_intvs ~cast:true ik x |> (function (intvs,underflow,overflow,cast) -> (canonize intvs, underflow,overflow,cast))

  (*
      narrows down the extremeties of xs if they are equal to boundary values of the ikind with (possibly) narrower values from ys
  *)
  let narrow ik xs ys = match xs ,ys with 
    | [], _ -> [] | _ ,[] -> xs
    | _, _ ->
      let min_xs = minimal xs |> Option.get in
      let max_xs = maximal xs |> Option.get in
      let min_ys = minimal ys |> Option.get in
      let max_ys = maximal ys |> Option.get in
      let min_range,max_range = range ik in
      let min = if Ints_t.compare min_xs min_range == 0 then min_ys else min_xs in
      let max = if Ints_t.compare max_xs max_range == 0 then max_ys else max_xs in
      xs 
      |> (function (_, y)::z -> (min, y)::z | _ -> []) 
      |> List.rev 
      |> (function (x, _)::z -> (x, max)::z | _ -> []) 
      |> List.rev 

  (*
    1. partitions the intervals of xs by assigning each of them to the an interval in ys that includes it.
     and joins all intervals in xs assigned to the same interval in ys as one interval.
    2. checks for every pair of adjacent pairs whether the pairs did approach (if you compare the intervals from xs and ys) and merges them if it is the case.
    3. checks whether partitions at the extremeties are approaching infinity (and expands them to infinity. in that case)

    The expansion (between a pair of adjacent partitions or at extremeties ) stops at a threshold.
  *)
  let widen ik xs ys = 
    let (min_ik,max_ik) = range ik in 
    let threshold = get_bool "ana.int.interval_threshold_widening" in
    let upper_threshold (_,u) =
      let ts = if GobConfig.get_string "ana.int.interval_threshold_widening_constants" = "comparisons" then WideningThresholds.upper_thresholds () else ResettableLazy.force IntDomain.widening_thresholds in
      let u = Ints_t.to_bigint u in
      let t = List.find_opt (fun x -> Z.compare u x <= 0) ts in
      BatOption.map_default Ints_t.of_bigint max_ik t
    in
    let lower_threshold (l,_) =
      let ts = if GobConfig.get_string "ana.int.interval_threshold_widening_constants" = "comparisons" then WideningThresholds.lower_thresholds () else ResettableLazy.force IntDomain.widening_thresholds_desc in
      let l = Ints_t.to_bigint l in
      let t = List.find_opt (fun x -> Z.compare l x >= 0) ts in
      BatOption.map_default Ints_t.of_bigint min_ik t
    in
    (*obtain partitioning of xs intervals according to the ys interval that includes them*)
    let rec interval_sets_to_partitions (ik: ikind) (acc : (int_t * int_t) option) (xs: t) (ys: t)= 
      match xs,ys with 
      | _, [] -> []
      | [], (y::ys) -> (acc,y):: interval_sets_to_partitions ik None [] ys 
      | (x::xs), (y::ys) when  Interval.leq (Some x) (Some y) -> interval_sets_to_partitions ik (Interval.join ik acc (Some x)) xs (y::ys)
      | (x::xs), (y::ys) -> (acc,y) :: interval_sets_to_partitions ik None  (x::xs) ys
    in 
    let interval_sets_to_partitions ik xs ys = interval_sets_to_partitions ik None xs ys in
    (*merge a pair of adjacent partitions*)
    let merge_pair ik (a,b) (c,d) =
      let new_a = function
        | None -> Some (upper_threshold b, upper_threshold b)
        | Some (ax,ay) -> Some (ax, upper_threshold b)
      in
      let new_c = function
        | None -> Some (lower_threshold d, lower_threshold d)
        | Some (cx,cy) -> Some (lower_threshold d, cy)
      in
      if threshold && Ints_t.compare (lower_threshold d) (upper_threshold b) > 1 then 
        [(new_a a,(fst b, upper_threshold b)); (new_c c, (lower_threshold d, snd d))] else
        [(Interval.join ik a c, (Interval.join ik (Some b) (Some d) |> Option.get))]
    in
    let partitions_are_approaching part_left part_right = match part_left, part_right with 
      | (Some (_, left_x), (_, left_y)), (Some (right_x, _), (right_y, _)) -> Ints_t.compare (Ints_t.sub right_x left_x) (Ints_t.sub right_y left_y) > 0  
      | _,_ -> false 
    in
    (*merge all approaching pairs of adjacent partitions*)
    let rec merge_list ik = function 
      | [] -> []
      | x::y::xs  when partitions_are_approaching x y -> merge_list ik ((merge_pair ik x y) @ xs)
      | x::xs -> x :: merge_list ik xs 
    in 
    (*expands left extremety*)
    let widen_left = function
      | [] -> []
      | (None,(lb,rb))::ts -> let lt = if threshold then lower_threshold (lb,lb) else min_ik in (None, (lt,rb))::ts
      | (Some (la,ra), (lb,rb))::ts  when Ints_t.compare lb la < 0 ->  let lt = if threshold then lower_threshold (lb,lb) else min_ik in (Some (la,ra),(lt,rb))::ts    | x  -> x
    in 
    (*expands right extremety*)
    let widen_right x = List.rev x |>  (function
        | [] -> []
        | (None,(lb,rb))::ts -> let ut = if threshold then upper_threshold (rb,rb) else max_ik in (None, (lb,ut))::ts
        | (Some (la,ra), (lb,rb))::ts  when Ints_t.compare ra rb < 0 -> let ut = if threshold then upper_threshold (rb,rb) else max_ik in (Some (la,ra),(lb,ut))::ts
        | x  -> x)|> List.rev
    in interval_sets_to_partitions ik xs ys |> merge_list ik |> widen_left |> widen_right |> List.map snd  

  let starting ?(suppress_ovwarn=false) ik n = norm_interval ik ~suppress_ovwarn (n, snd (range ik))

  let ending ?(suppress_ovwarn=false) ik n = norm_interval ik ~suppress_ovwarn (fst (range ik), n)

  let invariant_ikind e ik xs = 
    List.map (fun x -> Interval.invariant_ikind e ik (Some x)) xs |> 
    let open Invariant in List.fold_left (||) (bot ())

  let modulo n k =
    let result = Ints_t.rem n k in
    if Ints_t.compare result Ints_t.zero >= 0 then result
    else Ints_t.add result k

  let refine_with_congruence ik (intvs: t) (cong: (int_t * int_t ) option): t =
    let refine_with_congruence_interval ik (cong : (int_t * int_t ) option) (intv : (int_t * int_t ) option): t =
      match intv, cong with
      | Some (x, y), Some (c, m) ->
        if Ints_t.equal m Ints_t.zero && (Ints_t.compare c x < 0 || Ints_t.compare c y > 0) then []
        else if Ints_t.equal m Ints_t.zero then
          [(c, c)]
        else
          let (min_ik, max_ik) = range ik in
          let rcx =
            if Ints_t.equal x min_ik then x else
              Ints_t.add x (modulo (Ints_t.sub c x) (Ints_t.abs m)) in
          let lcy =
            if Ints_t.equal y max_ik then y else
              Ints_t.sub y (modulo (Ints_t.sub y c) (Ints_t.abs m)) in
          if Ints_t.compare rcx lcy > 0 then []
          else if Ints_t.equal rcx lcy then norm_interval ik (rcx, rcx) |> IntDomain.unlift
          else norm_interval ik (rcx, lcy) |> IntDomain.unlift
      | _ -> []
    in
    List.map (fun x -> Some x) intvs |> List.map (refine_with_congruence_interval ik cong) |> List.flatten

  let refine_with_interval ik xs = function None -> [] | Some (a,b) -> meet ik xs [(a,b)]

  let refine_with_incl_list ik intvs  = function
    | None -> intvs
    | Some xs -> meet ik intvs (List.map (fun x -> (x,x)) xs)

  let excl_range_to_intervalset (ik: ikind) ((min, max): int_t * int_t) (excl: int_t): t = 
    let intv1 = (min, Ints_t.sub excl Ints_t.one) in
    let intv2 = (Ints_t.add excl Ints_t.one, max) in
    norm_intvs ik ~suppress_ovwarn:true [intv1 ; intv2] |> IntDomain.unlift |> canonize

  let of_excl_list ik (excls: int_t list) = 
    let excl_list = List.map (excl_range_to_intervalset ik (range ik)) excls in
    let res = List.fold_left (meet ik) (top_of ik) excl_list in
    res

  let refine_with_excl_list ik (intv : t) = function
    | None -> intv
    | Some (xs, range) -> 
      let excl_to_intervalset (ik: ikind) ((rl, rh): (int64 * int64)) (excl: int_t): t = 
        excl_range_to_intervalset ik (Ints_t.of_bigint (IntDomain.Size.min_from_bit_range rl),Ints_t.of_bigint (IntDomain.Size.max_from_bit_range rh)) excl
      in
      let excl_list = List.map (excl_to_intervalset ik range) xs in 
      List.fold_left (meet ik) intv excl_list

  let project ik p t = t

  let arbitrary ik =
    let open QCheck.Iter in
    (* let int_arb = QCheck.map ~rev:Ints_t.to_bigint Ints_t.of_bigint MyCheck.Arbitrary.big_int in *)
    (* TODO: apparently bigints are really slow compared to int64 for domaintest *)
    let int_arb = QCheck.map ~rev:Ints_t.to_int64 Ints_t.of_int64 MyCheck.Arbitrary.int64 in
    let pair_arb = QCheck.pair int_arb int_arb in
    let list_pair_arb = QCheck.small_list pair_arb in
    let canonize_randomly_generated_list = (fun x -> norm_intvs ik  x |> IntDomain.unlift |> canonize) in
    let shrink xs = MyCheck.shrink list_pair_arb xs >|= canonize_randomly_generated_list
    in QCheck.(set_shrink shrink @@ set_print show @@ map (*~rev:BatOption.get*) canonize_randomly_generated_list list_pair_arb)

end