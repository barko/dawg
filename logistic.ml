open Proto_t

let logit ~f ~y =

  let f2 = -2.0 *. f in
  let ef2 = exp f2 in (* $\exp (-2 f)$ *)

  (* $\exp( -2 y f ) = (\exp (-2 f))^y $ *)
  let e =
    if y = 1.0 then
      ef2
    else
      (1. /. ef2)
  in

  let y2f = y *. f2 in
  let loss =
    (* $\log(1 + \exp x)) = x$ as $x \rightarrow \infinity$ *)
    if y2f > 35.0 then
      y2f
    else
      log ( 1.0 +. e )
  in

  (* let p = 1. /. ( 1. +. ef2 ) in *)

  match classify_float e with
    | FP_nan ->
      `NaN

    | FP_infinite ->
      (* in the limit *)
      let z = 2.0 *. y in
      let abs_z = abs_float z in
      let w = abs_z *. ( 2.0 -. abs_z ) in
      `Number (loss, z, w)

    | _ ->
      let z = ( 2.0 *. y *. e ) /. ( e +. 1.0 ) in
      let abs_z = abs_float z in
      let w = abs_z *. ( 2.0 -. abs_z ) in
      `Number (loss, z, w)


let probability f =
  let f2 = -2.0 *. f in
  let ef2 = exp f2 in
  1. /. ( 1. +. ef2 )


type metrics = {
  n : int;
  tt : float;
  tf : float;
  ft : float;
  ff : float;
  loss : float;
}

let loss { loss; tf; ft } =
  let frac_misclassified = tf +. ft in
  let has_converged = frac_misclassified = 0.0 in
  loss, has_converged

type model = Model_t.l_logistic_model

(*
type s = {
  (* probability: $p(\bf{ x_i } = Pr(y_i=1|\bf{ x_i } ) = 1/(1 + \exp( -2 f( \bf{x_i} ) ) ) *)
  p : float array;

}
*)


let string_of_metrics { n; loss; tt; tf; ft; ff } =
  Printf.sprintf "% 8d %.4e %.4e %.4e %.4e %.4e"
    n loss tt tf ft ff

exception WrongTargetType
exception BadTargetDistribution

let zero_one_to_minus_plus_one = function
  | 0 -> -.1.
  | 1 ->   1.
  | _ -> assert false

let y_array_of_cat n cat =
  let open Dog_t in
  let y = Array.create n nan in
  if cat.c_cardinality <> 2 then
    raise WrongTargetType;

  match cat.c_anonymous_category with
    | Some anon_value -> (
        assert (anon_value = 1 || anon_value = 0 );
        (* we want the anonymous category to be the negative
           one, and the named category to be positive *)
        let to_plus_minus_one value =
          if value = anon_value then
            -.1.0
          else
            1.0
        in
        match cat.c_vector with
          | `RLE (rle:Vec.t) ->
            Rlevec.iter rle (
              fun ~index ~length ~value ->
                let mp_one = to_plus_minus_one value in
                for i = index to index + length - 1 do
                  y.(i) <- mp_one
                done
            );

          | `Dense (vec:Vec.t) ->
            let width = Utils.num_bytes cat.c_cardinality in
            Dense.iter ~width vec (
              fun ~index ~value ->
                let mp_one = to_plus_minus_one value in
                y.(index) <- mp_one
            );
      );
      let named_category =
        match cat.c_categories with
          | [ cat ] -> cat
          | _ -> assert false
      in
      y, named_category, None

    | None -> (
        (* both categories are named; arbitrarily pick one as positive *)
        match cat.c_vector with
          | `RLE rle ->
            Rlevec.iter rle (
              fun ~index ~length ~value ->
                let mp_one = zero_one_to_minus_plus_one value in
                for i = index to index + length - 1 do
                  y.(i) <- mp_one
                done
            );

          | `Dense vec ->
            let width = Utils.num_bytes cat.c_cardinality in
            Dense.iter ~width vec (
              fun ~index ~value ->
                let mp_one = zero_one_to_minus_plus_one value in
                y.(index) <- mp_one
            );
      );
      match cat.c_categories with
        | [ cat_0; cat_1 ] ->
          y, cat_1, Some cat_0

        | _ -> assert false

let y_array_of_ord n ord =
  let open Dog_t in
  let { o_vector; o_breakpoints; o_cardinality } = ord in
  let y = Array.create n nan in
  let map, positive_category, negative_category_opt =
    if o_cardinality = 2 then
      match o_breakpoints with
        | `Float breakpoints -> (
            match breakpoints with
              | [v0; v1] ->
                zero_one_to_minus_plus_one,
                string_of_float v1,
                Some (string_of_float v0)
              | _ -> assert false
          )

        | `Int breakpoints -> (
            match breakpoints with
              | [v0; v1] ->
                zero_one_to_minus_plus_one,
                string_of_int v1,
                Some (string_of_int v0)
              | _ -> assert false
          )
    else
      raise WrongTargetType
  in

  (match o_vector with
    | `RLE rle -> (
        match o_breakpoints with
          | `Float breakpoints ->
            Rlevec.iter rle (
              fun ~index ~length ~value ->
                let mp_one = map value in
                for i = index to index + length - 1 do
                  y.(i) <- mp_one
                done
            );

          | `Int breakpoints ->
            Rlevec.iter rle (
              fun ~index ~length ~value ->
                let mp_one = map value in
                for i = index to index + length - 1 do
                  y.(i) <- mp_one
                done
            );
      )

    | `Dense vec -> (
        let width = Utils.num_bytes o_cardinality in
        match o_breakpoints with
          | `Float breakpoints ->
            Dense.iter ~width vec (
              fun ~index ~value ->
                let mp_one = map value in
                y.( index ) <- mp_one
            );

          | `Int breakpoints ->
            Dense.iter ~width vec (
              fun ~index ~value ->
                let mp_one = map value in
                y.( index ) <- mp_one
            );
      )
  );
  y, positive_category, negative_category_opt

let y_array_of_feature y_feature n =
  (* convert bools to {-1,+1} *)
  let y, p, n_opt =
    match y_feature with
      | `Cat cat -> y_array_of_cat n cat
      | `Ord ord -> y_array_of_ord n ord
  in
  assert (
    try
      for i = 0 to n-1 do
        match classify_float y.(i) with
          | FP_normal -> ()
          | _ -> raise Sys.Break
      done;
      true
    with Sys.Break ->
      false
  );
  y, p, n_opt

module Aggregate = struct
  type t = {
    sum_n : int array;
    sum_z : float array;
    sum_w : float array;
    sum_l : float array;
  }

  let update t ~value ~n ~z ~w ~l =
    t.sum_n.(value) <- t.sum_n.(value) +  n;
    t.sum_z.(value) <- t.sum_z.(value) +. z;
    t.sum_w.(value) <- t.sum_w.(value) +. w;
    t.sum_l.(value) <- t.sum_l.(value) +. l

  let create cardinality = {
    sum_n = Array.create cardinality 0;
    sum_z = Array.create cardinality 0.0;
    sum_w = Array.create cardinality 0.0;
    sum_l = Array.create cardinality 0.0;
  }

end

(* what would the sum_l be after the split is applied? *)
let updated_loss ~gamma  ~sum_l ~sum_z ~sum_w =
  sum_l -. gamma *. sum_z +. 0.5 *. gamma *. gamma *. sum_w

class splitter y_feature n =
  let y, positive_category, negative_category_opt =
    y_array_of_feature y_feature n in

  let z = Array.create n 0.0 in
  let w = Array.create n 0.0 in
  let l = Array.create n 0.0 in
  let f = Array.create n 0.0 in

  let n1 = n + 1 in

  let cum_z = Array.create n1 0.0 in
  let cum_w = Array.create n1 0.0 in
  let cum_l = Array.create n1 0.0 in
  let cum_n = Array.create n1 0 in

  let in_subset = ref [| |] in

  let update_cum () =
    cum_z.(0) <- 0.0;
    cum_w.(0) <- 0.0;
    cum_l.(0) <- 0.0;
    cum_n.(0) <- 0;

    for i = 1 to n do
      let i1 = i - 1 in
      if !in_subset.(i1) then (
        cum_z.(i) <- z.(i1) +. cum_z.(i1);
        cum_w.(i) <- w.(i1) +. cum_w.(i1);
        cum_l.(i) <- l.(i1) +. cum_l.(i1);
        cum_n.(i) <- 1      +  cum_n.(i1)
      )
      else (
        cum_z.(i) <- cum_z.(i1);
        cum_w.(i) <- cum_w.(i1);
        cum_l.(i) <- cum_l.(i1);
        cum_n.(i) <- cum_n.(i1)
      )
    done
  in

  let agg_of_vector cardinality = function
    | `RLE v ->
      let agg = Aggregate.create cardinality in
      Rlevec.iter v (
        fun ~index ~length ~value ->
          let index_length = index + length in

          let z_diff = cum_z.(index_length) -. cum_z.(index) in
          let w_diff = cum_w.(index_length) -. cum_w.(index) in
          let l_diff = cum_l.(index_length) -. cum_l.(index) in
          let n_diff = cum_n.(index_length) -  cum_n.(index) in

          assert ( n_diff >= 0 );

          Aggregate.update agg ~value ~n:n_diff ~l:l_diff
            ~z:z_diff ~w:w_diff
      );
      agg

    | `Dense v ->
      let agg = Aggregate.create cardinality in
      let width_num_bytes = Utils.num_bytes cardinality in
      Dense.iter ~width:width_num_bytes v (
        fun ~index ~value ->
          if !in_subset.(index) then
            Aggregate.update agg ~value ~n:1 ~l:l.(index)
              ~z:z.(index) ~w:w.(index)
      );
      agg

  in

  object
    method num_observations =
      n

    method clear =
      for i = 0 to n-1 do
        z.(i) <- 0.0;
        w.(i) <- 0.0;
        l.(i) <- 0.0;
        f.(i) <- 0.0;
        cum_z.(i) <- 0.0;
        cum_w.(i) <- 0.0;
        cum_l.(i) <- 0.0;
        cum_n.(i) <- 0;
      done;
      (* cum's have one more element *)
      cum_z.(n) <- 0.0;
      cum_w.(n) <- 0.0;
      cum_l.(n) <- 0.0;
      cum_n.(n) <- 0;
      in_subset := [| |]

    (* update [f] and [zwl] based on [gamma] *)
    method boost gamma : [ `NaN | `Ok ] =
      let last_nan = ref None in
      Array.iteri (
        fun i gamma_i ->

          (* update [f.(i)] *)
          f.(i) <- f.(i) +. gamma_i;

          let li, zi, wi =
            match logit ~f:f.(i) ~y:y.(i) with
              | `Number lzw -> lzw
              | `NaN ->
                last_nan := Some i;
                (nan,nan,nan)
          in

          z.(i) <- zi;
          w.(i) <- wi;
          l.(i) <- li;

      ) gamma;
      match !last_nan with
        | Some _ -> `NaN
        | None -> `Ok

    method update_with_subset in_subset_ =
      in_subset := in_subset_;
      update_cum ()

    method best_split feature : (float * Proto_t.split) option =
      let feature_id = Feat_utils.id_of_feature feature in

      let open Aggregate in
      let open Dog_t in
      let cardinality, kind, agg =
        match feature with
          | `Ord { o_cardinality; o_vector; o_feature_id } ->
            let agg = agg_of_vector o_cardinality o_vector in
            o_cardinality, `Ord, agg

          | `Cat { c_cardinality; c_vector; c_feature_id } ->
            let agg = agg_of_vector c_cardinality c_vector in
            c_cardinality, `Cat, agg
      in

      let left = Aggregate.create cardinality in
      let right = Aggregate.create cardinality in

      match kind with
        | `Cat ->

          (* categorical feature: find the partition resulting in the
             minimum loss. *)

          (* sort the levels by sum_z/n -- which is the average of the
             pseudo response's *)
          let pseudo_response_sorted =
            Array.init cardinality (
              fun k ->
                let n = float_of_int agg.sum_n.(k) in
                let average_response = agg.sum_z.(k) /. n in
                k, average_response
            )
          in
          (* now, [pseudo_respones_sorted] is not really sorted yet.
               this sorts it in place: *)
          Array.sort (
            fun (_,avg_z1) (_,avg_z2) ->
              Pervasives.compare avg_z1 avg_z2
          ) pseudo_response_sorted;
          (* phew:  now [pseudo_respone_sorted] is really sorted *)

          (* [s] is index into the array of
             [pseudo_resopnse_sorted] *)
          let s_to_k = Array.init cardinality (
              fun s ->
                let k, _ = pseudo_response_sorted.(s) in
                k
            ) in

          let k_0    = s_to_k.(0) in
          let k_last = s_to_k.(cardinality-1) in

          (* initialize the cumulative sums from left to right *)
          left.sum_n.(k_0) <- agg.sum_n.(k_0);
          left.sum_z.(k_0) <- agg.sum_z.(k_0);
          left.sum_w.(k_0) <- agg.sum_w.(k_0);
          left.sum_l.(k_0) <- agg.sum_l.(k_0);

          right.sum_n.(k_last) <- agg.sum_n.(k_last);
          right.sum_z.(k_last) <- agg.sum_z.(k_last);
          right.sum_w.(k_last) <- agg.sum_w.(k_last);
          right.sum_l.(k_last) <- agg.sum_l.(k_last);

          (* compute the cumulative sums from left to right *)
          for ls = 1 to cardinality-1 do

            let lk   = s_to_k.(ls)   in
            let lk_1 = s_to_k.(ls-1) in

            left.sum_n.(lk) <- left.sum_n.(lk_1) +  agg.sum_n.(lk);
            left.sum_z.(lk) <- left.sum_z.(lk_1) +. agg.sum_z.(lk);
            left.sum_w.(lk) <- left.sum_w.(lk_1) +. agg.sum_w.(lk);
            left.sum_l.(lk) <- left.sum_l.(lk_1) +. agg.sum_l.(lk);

            let rs = cardinality - ls - 1 in
            let rk   = s_to_k.(rs)   in
            let rk_1 = s_to_k.(rs+1) in

            right.sum_n.(rk) <- right.sum_n.(rk_1) +  agg.sum_n.(rk);
            right.sum_z.(rk) <- right.sum_z.(rk_1) +. agg.sum_z.(rk);
            right.sum_w.(rk) <- right.sum_w.(rk_1) +. agg.sum_w.(rk);
            right.sum_l.(rk) <- right.sum_l.(rk_1) +. agg.sum_l.(rk);

          done;

          let best_split = ref None in

          (* find and keep optimal split -- the one associated with the
             minimum loss *)
          for s = 0 to cardinality-2 do

            let k   = s_to_k.(s)   in
            let k_1 = s_to_k.(s+1) in

            let left_n  = left.sum_n.(k)    in
            let right_n = right.sum_n.(k_1) in

            (* we can only have a split when the left and right
               approximations are based on one or more observations *)

            if left_n > 0 &&
               right_n > 0 &&
               left.sum_w.(k) <> 0.0 &&
               right.sum_w.(k_1) <> 0.0
            then (

              let left_gamma  = left.sum_z.(k)    /. left.sum_w.(k)    in
              let right_gamma = right.sum_z.(k_1) /. right.sum_w.(k_1) in

              let loss_left = updated_loss
                  ~gamma:left_gamma
                  ~sum_l:left.sum_l.(k)
                  ~sum_z:left.sum_z.(k)
                  ~sum_w:left.sum_w.(k)
              in

              let loss_right = updated_loss
                  ~gamma:right_gamma
                  ~sum_l:right.sum_l.(k_1)
                  ~sum_z:right.sum_z.(k_1)
                  ~sum_w:right.sum_w.(k_1)
              in

              let total_loss = loss_left +. loss_right in

              let is_total_loss_smaller =
                match !best_split with
                  | None -> true
                  | Some (best_total_loss, best_split) ->
                    total_loss < best_total_loss
              in

              if is_total_loss_smaller then
                let left = {
                  s_n = left_n ;
                  s_gamma = left_gamma ;
                  s_loss = loss_left;
                }
                in

                let right = {
                  s_n = right_n ;
                  s_gamma = right_gamma ;
                  s_loss = loss_right;
                }
                in

                let ord_split = {
                  os_feature_id = feature_id;
                  os_split = s;
                  os_left = left;
                  os_right = right;
                } in

                let split = `CategoricalSplit (ord_split, s_to_k) in
                best_split := Some (total_loss, split)
            )
          done;
          !best_split

        | `Ord ->

          let last = cardinality - 1 in

          (* initialize the cumulative sums in each direction *)
          left.sum_n.(0) <- agg.sum_n.(0);
          left.sum_w.(0) <- agg.sum_w.(0);
          left.sum_z.(0) <- agg.sum_z.(0);
          left.sum_l.(0) <- agg.sum_l.(0);

          right.sum_n.(last) <- agg.sum_n.(last);
          right.sum_w.(last) <- agg.sum_w.(last);
          right.sum_z.(last) <- agg.sum_z.(last);
          right.sum_l.(last) <- agg.sum_l.(last);

          (* compute the cumulative sums *)
          for lk = 1 to last do
            left.sum_n.(lk) <- left.sum_n.(lk-1) +  agg.sum_n.(lk);
            left.sum_z.(lk) <- left.sum_z.(lk-1) +. agg.sum_z.(lk);
            left.sum_w.(lk) <- left.sum_w.(lk-1) +. agg.sum_w.(lk);
            left.sum_l.(lk) <- left.sum_l.(lk-1) +. agg.sum_l.(lk);

            let rk = cardinality - lk - 1 in
            right.sum_n.(rk) <- right.sum_n.(rk+1) +  agg.sum_n.(rk);
            right.sum_z.(rk) <- right.sum_z.(rk+1) +. agg.sum_z.(rk);
            right.sum_w.(rk) <- right.sum_w.(rk+1) +. agg.sum_w.(rk);
            right.sum_l.(rk) <- right.sum_l.(rk+1) +. agg.sum_l.(rk);
          done;

          let best_split = ref None in

          (* find and keep optimal split -- the one associated with the minimum loss *)
          for k = 0 to cardinality-2 do
            let left_n  = left.sum_n.(k)    in
            let right_n = right.sum_n.(k+1) in

            (* we can only have a split when the left and right
               approximations are based on one or more observations *)

            if left_n > 0 &&
               right_n > 0 &&
               left.sum_w.(k) <> 0.0 &&
               right.sum_w.(k+1) <> 0.0
            then (

              let left_gamma  = left.sum_z.(k)    /. left.sum_w.(k)    in
              let right_gamma = right.sum_z.(k+1) /. right.sum_w.(k+1) in

              let loss_left = updated_loss
                  ~gamma:left_gamma
                  ~sum_l:left.sum_l.(k)
                  ~sum_z:left.sum_z.(k)
                  ~sum_w:left.sum_w.(k)
              in

              let loss_right = updated_loss
                  ~gamma:right_gamma
                  ~sum_l:right.sum_l.(k+1)
                  ~sum_z:right.sum_z.(k+1)
                  ~sum_w:right.sum_w.(k+1)
              in

              let total_loss = loss_left +. loss_right in

              let is_total_loss_smaller =
                match !best_split with
                  | None -> true
                  | Some (best_total_loss, best_split) ->
                    total_loss < best_total_loss
              in

              if is_total_loss_smaller then
                let left = {
                  s_n = left_n ;
                  s_gamma = left_gamma ;
                  s_loss = loss_left;
                }
                in

                let right = {
                  s_n = right_n ;
                  s_gamma = right_gamma ;
                  s_loss = loss_right;
                }
                in

                let curr_split = `OrdinalSplit {
                    os_feature_id = feature_id;
                    os_split = k ;
                    os_left = left ;
                    os_right = right ;
                  }
                in
                best_split := Some (total_loss, curr_split)
            )
          done;
          !best_split

    method metrics mem =
      let tt = ref 0 in
      let tf = ref 0 in
      let ft = ref 0 in
      let ff = ref 0 in
      let loss = ref 0.0 in
      let nn = ref 0 in

      for i = 0 to n-1 do
        if mem i then
          let cell =
            match y.(i) >= 0.0, f.(i) >= 0.0 with
              | true , true  -> tt
              | true , false -> tf
              | false, true  -> ft
              | false, false -> ff
          in
          incr cell;
          incr nn;
          loss := !loss +. l.(i)
      done;

      let nf = float !nn in
      if !nn > 0 then
        let metrics = {
          n = !nn ;
          tt = (float !tt) /. nf ;
          tf = (float !tf) /. nf ;
          ft = (float !ft) /. nf ;
          ff = (float !ff) /. nf ;
          loss = !loss /. nf ;
        } in
        Some metrics
      else
        None

    method first_tree set : Model_t.l_tree =
      assert (Array.length set = n);
      let n_true = ref 0 in
      for i = 0 to n-1 do
        if set.(i) then
          match y.(i) with
            |  1.0 -> incr n_true
            | -1.0 -> ()
            | _ -> assert false
      done;
      let n_true = !n_true in
      let n_false = n - n_true in
      if n_false = 0 || n_true = 0 then
        raise BadTargetDistribution
      else
        let mean_y = (float_of_int (n_true - n_false)) /. (float_of_int n)  in
        let gamma0 = 0.5 *. (log ((1.0 +. mean_y) /. (1.0 -. mean_y))) in
        `Leaf gamma0

    method write_model trees features out_buf =
      let open Model_t in
      let model = `Logistic {
        bi_positive_category = positive_category;
        bi_negative_category_opt = negative_category_opt;
        bi_trees = trees;
        bi_features = features;
      } in
      Model_j.write_c_model out_buf model

  end

