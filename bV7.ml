type uniform = bool * int

type run = Uniform of uniform | Literal of int

let max_length_of_run = function
  | Uniform (_, length) -> length
  | Literal _ -> 7
      (* when a bitvector is terminated by a trainling literal, then
         its length could be anywhere from one to six bits *)

let string_of_run = function
  | Literal w -> Printf.sprintf "Literal %d" w
  | Uniform (bit, length) -> Printf.sprintf "Uniform (%b,%d)" bit length

let string_of_runs runs =
  String.concat "; " (List.map string_of_run runs)

open Bigarray
type uint8_array = (int, int8_unsigned_elt, c_layout) Array1.t

module UInt8Array = struct
  type t = uint8_array

  let rec map_to_list f t i accu =
    if i < 0 then
      accu
    else
      let accu = (f t.{i}) :: accu in
      map_to_list f t (i-1) accu

  let map_to_list f (t:t) =
    let n = Array1.dim t in
    map_to_list f t (n - 1) []

  let length t =
    Array1.dim t

  let create n : uint8_array =
    Array1.create int8_unsigned c_layout n

  let sub = Array1.sub

  let char_iter_i f t =
    for i = 0 to (Array1.dim t)-1 do
      f i (Char.chr t.{i})
    done

end

exception Error

let read_run i (bv:uint8_array) =
  let c0 = bv.{i} in
  if c0 land 0b1000_0000 = 0 then
    1, Literal (0b0111_1111 land c0)
  else
    let uniform_value = (c0 land 0b0100_0000) != 0 in
    let num_blocks_encoded_in_more_bytes =
      (c0 land 0b00_1_00000) = 0b00_1_00000 in

    if num_blocks_encoded_in_more_bytes then
      let num_bytes = 1 + ((c0 land 0b000_11_000) lsr 3) in

      (* set the three most significant bits of the length.  The
         maximum number of bits allocated to the num_blocks field is (3 +
         4 * 8) = 35, allowing for a maximum value of 2^35 ~= 34
         billion, and equivalently, a maximum length 7 time that.  *)
      let num_blocks = (c0 land 0b0000_0111) lsl 8 in
      let num_blocks =
        match num_bytes with
          | 1 ->
              let c1 = bv.{i+1} in
              num_blocks lor c1

          | 2 ->
              let c1 = bv.{i+1} in
              let c2 = bv.{i+2} in
              ((num_blocks lor c1) lsl 8) lor c2

          | 3 ->
              let c1 = bv.{i+1} in
              let c2 = bv.{i+2} in
              let c3 = bv.{i+3} in
              ((((num_blocks lor c1) lsl 8) lor c2) lsl 8) lor c3

          | 4 ->
              let c1 = bv.{i+1} in
              let c2 = bv.{i+2} in
              let c3 = bv.{i+3} in
              let c4 = bv.{i+4} in
              ((((((num_blocks lor c1) lsl 8) lor c2) lsl 8) lor c3) lsl 8) lor c4

          | _ ->
              raise Error

      in
      let length = 7 * num_blocks in
      1 + num_bytes, Uniform (uniform_value, length)

    else
      let num_blocks = c0 land 0b0001_1111 in
      let length = 7 * num_blocks in
      1, Uniform (uniform_value, length)

let write_run i (bv:uint8_array) = function
  | Literal w ->
      assert ( 0 <= w && w < 128 ); (* a trailing Literal run can be uniformly zero's *)
      bv.{i} <- w;
      1

  | Uniform (value, length) ->
      assert (length mod 7 = 0);
      let num_blocks = length / 7 in
      let c0 =
        if value then
          (* represents a sequence of 1's *)
          0b1100_0000
        else
          (* represents a sequence of 0's *)
          0b1000_0000
      in

      if num_blocks < 32 then
        (* we can encode the length in 5 bits *)
        let c0 = c0 lor num_blocks in
        bv.{i} <- c0;
        1

      else
        if num_blocks < 0x7_ff then
          let c0 = c0 lor 0b00_1_00_000 in
          (* add the three most significant bits of [num_blocks] to
             the last three bits of [c0] *)
          let c0 = c0 lor (num_blocks lsr 8) in
          let c1 = num_blocks land 0xff in
          bv.{i  } <- c0;
          bv.{i+1} <- c1;
          2

        else if num_blocks < 0x7_ff_ff then
          let c0 = c0 lor 0b00_1_01_000 in
          let c0 = c0 lor (num_blocks lsr 16) in
          let c1 = (num_blocks land 0x0_ff_00) lsr 8 in
          let c2 = num_blocks land 0x0_00_ff in
          bv.{i  } <- c0;
          bv.{i+1} <- c1;
          bv.{i+2} <- c2;
          3

        else if num_blocks < 0x7_ff_ff_ff then
          let c0 = c0 lor 0b00_1_10_000 in
          let c0 = c0 lor (num_blocks lsr 24) in
          let c1 = (num_blocks land 0x0_ff_00_00) lsr 16 in
          let c2 = (num_blocks land 0x0_00_ff_00) lsr 8 in
          let c3 = num_blocks land 0x0_00_00_ff in
          bv.{i  } <- c0;
          bv.{i+1} <- c1;
          bv.{i+2} <- c2;
          bv.{i+3} <- c3;
          4

        else if num_blocks < 0x7_ff_ff_ff_ff then
          let c0 = c0 lor 0b00_1_11_000 in
          let c0 = c0 lor (num_blocks lsr 32) in
          let c1 = (num_blocks land 0x0_ff_00_00_00) lsr 24 in
          let c2 = (num_blocks land 0x0_00_ff_00_00) lsr 16 in
          let c3 = (num_blocks land 0x0_00_00_ff_00) lsr 8 in
          let c4 = num_blocks land 0x0_00_00_00_ff in
          bv.{i  } <- c0;
          bv.{i+1} <- c1;
          bv.{i+2} <- c2;
          bv.{i+3} <- c3;
          bv.{i+4} <- c4;
          5

        else
          (* [num_blocks] is greater than or equal to
             [0x7_ff_ff_ff_ff] = [34359738367] *)
          raise Error


type e = {
  offset : int;
  (* where in {a} does the bitvector encoding begin? *)

  num_bytes : int;
  (* how many bytes are used to encode the bitvector in UInt8Array [a] ? *)

  e_length : int;
  (* how many bits are in the bitvector? *)

  a : UInt8Array.t
}

type d = {
  (* how many bits are in the bitvector? *)
  d_length : int;

  (* each run represents a sequence of bits, whose length is a
     multiple of 7.  a [Uniform] run represent a sequence of identical
     bits.  a [Literal] run represents are sequence of 7 bits,
     containing at at least one 'one' bit and at least one 'zero' bit
     (that is, a [Literal] run is not uniform. *)
  runs : run list;

  (* what are the last few bits of the bit vector?  This value is
     meaningful only when the length of the vector [d_length] is not a
     multiple of 7.  In that case, the last [d_length mod 7] bits of the
     vector are encoded in this integer *)
  trailing_literal : int;
}

let string_of_d { d_length ; runs; trailing_literal } =
  Printf.sprintf "{d_length=%d; trailing_literal=%d; runs=[%s]}"
    d_length trailing_literal
    (String.concat "; " (List.map string_of_run runs))

let d_length { d_length } = d_length
let e_length { e_length } = e_length

let work_factor { runs ; d_length } =
  let num_trailing_bits = d_length mod 7 in
  let n_uniform, n_literal =
    List.fold_left (
      fun (n_uniform, n_literal) run ->
        match run with
          | Uniform _ -> n_uniform + 1, n_literal
          | Literal _ -> n_uniform    , n_literal + 1
    ) (0,0) runs in
  num_trailing_bits + (n_literal * 7) + n_uniform

let density d =
  (float (work_factor d)) /. (float d.d_length)

let char_iter_i f e =
  UInt8Array.char_iter_i f e.a

let literal_of_bit = function
  | false -> 0           (* seven zero's *)
  | true  -> 0b0111_1111 (* seven one's *)

let uniform bit d_length =
  let num_trailing_bits = d_length mod 7 in
  let num_uniform_bits = d_length - num_trailing_bits in
  let num_uniform_blocks = num_uniform_bits / 7 in
  let runs =
    if num_uniform_blocks > 0 then
      [Uniform (bit, num_uniform_bits)]
    else
      []
  in
  let trailing_literal = literal_of_bit bit in
  { d_length; runs; trailing_literal }


(* convert [d] to a newly created [e] *)
let encode { d_length; runs; trailing_literal } =
  let has_trailing_literal = (d_length mod 7) > 0 in

  let a = UInt8Array.create 5 in
  (* first determine how many bytes are required to encode the runs;
     fake-write them into a scratch array.  [5] is the maximum number
     of bytes necessary to encode a run *)

  let num_bytes = List.fold_left (
      fun num_bytes run ->
        let num_bytes_written = write_run 0 a run in
        num_bytes + num_bytes_written
    ) 0 runs in

  let num_bytes = num_bytes + (
    if has_trailing_literal then
      1
    else
      0
  ) in

  (* create an array of precisely the right length *)
  let a = UInt8Array.create num_bytes in

  (* now, in this second pass, actually write the runs into the array *)
  let offset = List.fold_left (
    fun offset run ->
      let num_bytes_written = write_run offset a run in
      offset + num_bytes_written
  ) 0 runs in

  if has_trailing_literal then
    ignore ( write_run offset a (Literal trailing_literal) );

  { e_length = d_length ;
    a;
    offset = 0; (* the bitvector encoding starts at index 0 of array [a] *)
    num_bytes;
  }

let decode { e_length ; a ; offset; num_bytes } =
  let fini = offset + num_bytes in
  let rec loop i bit_index bits_remaining accu =
    if i = fini then
      bit_index, bits_remaining, accu
    else
      let num_bytes_read, run = read_run i a in

      let bit_index, bits_remaining =
        match run with
          | Uniform (value, length) ->
            bit_index + length, bits_remaining - length

          | Literal literal ->
            let length = min bits_remaining 7 in
            bit_index + length, bits_remaining - length
      in

      let accu = run :: accu in
      let i = i + num_bytes_read in
      loop i bit_index bits_remaining accu
  in
  let bit_index, bits_remaining, runs_rev = loop offset 0 e_length [] in
  assert
    begin
      if bit_index = e_length && bits_remaining = 0 then
        true
      else (
        Printf.printf "BV7.decode: num_bytes=%d e_length=%d \
          bit_index=%d bits_remaining=%d\n%!"
          num_bytes e_length bit_index bits_remaining;
        false
      )
    end;

  (* check that, if the last run is a Uniform one, then its length is
     a multiple of 7 *)
  match runs_rev with
    | [] -> assert false
    | Uniform (_, u_length) :: _ ->
      (* there's no trailing literal *)
      assert (u_length mod 7 = 0);
      { runs = List.rev runs_rev; d_length = e_length; trailing_literal = 0; }

    | Literal trailing_literal :: rest ->
      if e_length mod 7 = 0 then
        { runs = List.rev runs_rev; d_length = e_length; trailing_literal = 0; }
      else
        (* strip the trailing literal, put in the record field *)
        { runs = List.rev rest; d_length = e_length; trailing_literal }


let int_of_bool = function
  | false -> 0
  | true -> 1

let int_of_bools_rev =
  let rec int_of_bools_rev probe v bit_index = function
    | [] -> v
    | h :: t ->
        assert (bit_index < 7);
        let v =
          if h then
            v lor probe
          else
            v
        in
        int_of_bools_rev (probe lsl 1) v (bit_index + 1) t
  in

  fun n bools_rev ->
    int_of_bools_rev (0b1000_0000 lsr n) 0 0 bools_rev

let add_bools_run n_false n_true block_rev = function
  | num_runs, ((Uniform (value, length)) as uniform) :: rest ->
      if value then
        if n_false = 0 then
          (* extend last uniform, which is a '1' uniform *)
          num_runs, Uniform (true, length + n_true) :: rest
        else if n_true = 0 then
          (* new '0' uniform *)
          num_runs + 1, Uniform (false, n_false) :: uniform :: rest
        else
          let i = int_of_bools_rev (n_false + n_true) block_rev in
          num_runs + 1, (Literal i) :: uniform :: rest

      else
        if n_true = 0 then
          (* extend last uniform, which is a '0' uniform *)
          num_runs, Uniform (false, length + n_false) :: rest
        else if n_false = 0 then
          (* new '1' uniform *)
          num_runs + 1, Uniform (true, n_true) :: uniform :: rest
        else
          let i = int_of_bools_rev (n_false + n_true) block_rev in
          num_runs + 1, (Literal i) :: uniform :: rest

  | num_runs, list ->
      let run =
        if n_false = 0 then
          Uniform (true, n_true)
        else if n_true = 0 then
          Uniform (false, n_false)
        else
          let i = int_of_bools_rev (n_false + n_true) block_rev in
          Literal i
      in
      num_runs + 1, run :: list

let decoded_of_bools =
  let rec loop i n n_false n_true block_rev runs =
    function
      | [] ->
        let num_runs, runs = add_bools_run n_false n_true block_rev runs in
        n, num_runs, runs

      | h :: t ->
        let n = n + 1 in
        if i = 7 then
          let runs = add_bools_run n_false n_true block_rev runs in

          (* start a new run *)
          let n_false, n_true =
            if h then
              0, 1
            else
              1, 0
          in
          loop 1 n n_false n_true [h] runs t

        else

          let n_false, n_true =
            if h then
              n_false, n_true + 1
            else
              n_false + 1, n_true
          in
          (* continue existing run *)
          let block_rev = h :: block_rev in
          loop (i+1) n n_false n_true block_rev runs t
  in
  fun bools ->
    let length, num_runs, runs_rev = loop 0 0 0 0 [] (0, []) bools in
    let num_trailing_bits = length mod 7 in
    if num_trailing_bits = 0 then
      { d_length = length; runs = List.rev runs_rev; trailing_literal = 0 }
    else
      match runs_rev with
        | [] -> assert false

        | Literal trailing_literal :: rest ->
          { d_length = length; runs = List.rev rest; trailing_literal }

        (* if there are trailing bits, we want the last run to be a a
           Literal; that way, we maintain the property that the length
           of a Uniform run is always a multiple of 7. *)
        | Uniform (bit, u_length) :: rest ->
          let u_length = u_length - num_trailing_bits in
          let normalized_uniform = Uniform (bit, u_length) in

          let trailing_literal =
            if bit then
              0b0111_1111 (* seven one's *)
            else
              0 (* seven zero's *)
          in
          let runs_rev =
            if u_length = 0 then (
              (* there's only one run! drop it. *)
              rest
            )
            else
              normalized_uniform :: rest
          in
          { d_length = length;
            runs = List.rev runs_rev;
            trailing_literal
          }

let bools_of_decoded =
  let rec add_from_bits width w accu i probe =
    if i = width then
      accu
    else
      let b = (w land probe) <> 0 in
      add_from_bits width w (b :: accu) (i+1) (probe lsr 1)
  in

  let add_from_bits width w accu =
    add_from_bits width w accu 0 0b0100_0000
  in

  let rec add_repeat length b accu =
    if length = 0 then
      accu
    else
      add_repeat (length-1) b (b :: accu)
  in

  let rec loop accu = function
    | [] -> accu

    | Uniform (bit, length) :: rest ->
      let accu = add_repeat length bit accu in
      loop accu rest

    | Literal w :: rest ->
      let accu = add_from_bits 7 w accu in
      loop accu rest
  in
  fun { d_length ; runs ; trailing_literal } ->
    let bools = loop [] runs in
    let num_trailing_bits = d_length mod 7 in
    let bools =
      if num_trailing_bits = 0 then
        (* nothing to add, as the trailing literal is empty *)
        bools
      else
        add_from_bits num_trailing_bits trailing_literal bools
    in
    List.rev bools

let decoded_of_indexes bit indexes length =
  let not_bit = not bit in
  let literal_not_bit = literal_of_bit not_bit in

  let rec loop prev_index runs_rev = function
    | [] ->
      (* terminate the bit vector *)
      let num_blocks = Utils.divide_up length 7 in
      let last_block = num_blocks - 1 in
      let num_trailing_bits = length mod 7 in
      let prev_block = prev_index / 7 in
      let diff_blocks = last_block - prev_block in

      let trailing_literal, runs_rev =
        if num_trailing_bits = 0 then
          let runs_rev =
            if diff_blocks = 0 then
              runs_rev
            else
              match runs_rev with
                | Literal _ :: _ ->
                  let uniform_spacer = Uniform (not_bit, diff_blocks * 7) in
                  uniform_spacer :: runs_rev

                | Uniform (u_bit, u_len) :: rr ->
                  if u_bit = not_bit then
                    (* merge *)
                    Uniform (not_bit, u_len + diff_blocks * 7) :: rr
                  else
                    let uniform_spacer = Uniform (not_bit, diff_blocks * 7) in
                    uniform_spacer :: runs_rev

                | [] -> assert false
          in
          0, runs_rev
        else (
          (* num_trailing_bits <> 0 *)
          if diff_blocks = 0 then
            match runs_rev with
              | Literal w :: rr ->  w, rr
              | Uniform _ :: _ -> assert false
              | [] -> assert false

          else if diff_blocks = 1 then
            literal_not_bit, runs_rev

          else
            match runs_rev with
              | Literal _ :: _ ->
                let uniform_spacer = Uniform (not_bit, (diff_blocks - 1) * 7) in
                literal_not_bit, uniform_spacer :: runs_rev

              | Uniform (u_bit, u_len) :: rr ->
                if u_bit = not_bit then
                  (* merge *)
                  let u_len = u_len + (diff_blocks - 1) * 7 in
                  literal_not_bit, Uniform (not_bit, u_len) :: rr
                else
                  let u_len = (diff_blocks - 1) * 7 in
                  let uniform_spacer = Uniform (not_bit, u_len) in
                  literal_not_bit, uniform_spacer :: runs_rev

              | [] -> assert false
        )
      in
      let runs = List.rev runs_rev in
      (* TODO: normalize the trailing literal, so that the unused bits
         are zero; fi this is not done, equality tests on decoded
         bitvectors will fail. *)
      { d_length = length; runs; trailing_literal }

    | index :: rest ->
      (* indexes must be strictly ascending *)
      if index <= prev_index then
        raise (Invalid_argument "decoded_from_indexes");

      let block = index / 7 in
      let rem = index mod 7 in

      let prev_block = prev_index / 7 in
      (* how many uniform blocks separate index and prev_index ? *)
      let diff_blocks = block - prev_block in

      let runs_rev =
        match runs_rev with
          | [] ->
            assert false

          | Literal w :: rest_runs_rev ->

            if diff_blocks = 0 then
              (* update this literal *)
              let w = w lxor (1 lsl (6 - rem)) in
              (Literal w) :: rest_runs_rev

            else if diff_blocks = 1 then
              (* add new literal run, but first check whether the last
                 literal is actually uniform, and if so, whether it can be
                 merged with the previous uniform *)
              let runs_rev =
                if w = 0 then
                  match rest_runs_rev with
                    | Uniform (false, u_len) :: rr ->
                      (* extend the last uniform *)
                      Uniform (false, u_len + 7) :: rr
                    | _ ->
                      Uniform (false, 7) :: rest_runs_rev
                else if w = 0b0111_1111 then
                  match rest_runs_rev with
                    | Uniform (true, u_len) :: rr ->
                      (* extend the last uniform *)
                      Uniform (true, u_len + 7) :: rr
                    | _ ->
                      Uniform (true, 7) :: rest_runs_rev
                else
                  (Literal w) :: rest_runs_rev
              in

              let w = literal_not_bit lxor (1 lsl (6 - rem)) in
              (Literal w) :: runs_rev

            else
              (* add new uniform run *)
              let unif_spacer = Uniform (not_bit, (diff_blocks - 1) * 7) in
              let w = literal_not_bit lxor (1 lsl (6 - rem)) in
              (Literal w) :: unif_spacer :: runs_rev

          | Uniform _ :: _ ->
            (* add new literal run *)
            assert (diff_blocks > 0);
            if diff_blocks = 1 then
              let w = literal_not_bit lxor (1 lsl (6 - rem)) in
              (Literal w) :: runs_rev
            else
              let unif_spacer = Uniform (not_bit, (diff_blocks - 1) * 7) in
              let w = literal_not_bit lxor (1 lsl (6 - rem)) in
              (Literal w) :: unif_spacer :: runs_rev

      in
      loop index runs_rev rest
  in
  match indexes with
    | [] ->
      uniform not_bit length

    | index :: rest ->
      let block = index / 7 in
      let rem = index mod 7 in
      let w = literal_not_bit lxor (1 lsl (6 - rem)) in
      let runs_rev =
        if block = 0 then
          [Literal w]
        else
          [Literal w; Uniform (not_bit, block * 7)]
      in
      loop index runs_rev rest

(* add a run to a sequence of runs (at its head), possibly merging the
   run with the head of the sequence *)
let rec add_run run (num_runs, runs) =
  match run, runs with
    | Literal _, []
    | Uniform _, [] -> 1, [run] (* first *)
    | Literal lit, _ ->
        if lit = 0 then
          (* a Literal of all 0's : convert to Uniform ; of course, we
             are not totally sure of the length, because this might be a
             trailing Literal; but we'll check and fix this later. *)
          add_run (Uniform (false, 7)) (num_runs, runs)
        else if lit = 0b0111_1111 then
          (* a Literal of all 1's : convert to Uniform *)
          add_run (Uniform (true, 7)) (num_runs, runs)
        else
          (* don't merge *)
          num_runs + 1, run :: runs

    | Uniform (v_a, len_a), (Uniform (v_b, len_b)) :: tail ->
        if v_a = v_b then
          (* merge *)
          num_runs, Uniform (v_a, len_a + len_b) :: tail
        else
          (* don't merge *)
          num_runs + 1, run :: runs

    | Uniform (v_a, len_a), (Literal lit_a) :: tail ->
        if v_a && lit_a = 0b0111_1111 then
          (* merge *)
          num_runs, Uniform (true, len_a + 7) :: tail
        else if not v_a && lit_a = 0 then
          (* merge *)
          num_runs, Uniform (false, len_a + 7) :: tail
        else
          (* don't merge *)
          num_runs + 1, run :: runs

let merge_runs run prev_run =
  match run, prev_run with
    | Literal lit_a, Literal lit_b ->
      assert (lit_a <> 0);
      assert (lit_b <> 0);
      assert (lit_a <> 0b0111_1111);
      assert (lit_b <> 0b0111_1111);
      None

    | Uniform (v_a, len_a), Literal lit
    | Literal lit, Uniform (v_a, len_a) ->
      assert (lit <> 0);
      assert (lit <> 0b0111_1111);
      None

    | Uniform (v_a, len_a), Uniform (v_b, len_b) ->
      if v_a = v_b then
        Some (Uniform (v_a, len_a + len_b))
      else
        None


and iter_run fu fl = function
  | _ -> assert false

type binary_op = XOR | OR | AND | AND_NOT

let f_of_binary_op = function
  | XOR -> ( lxor )
  | OR -> ( lor  )
  | AND -> ( land )
  | AND_NOT ->
      fun w1 w2 ->
        w1 land ( w2 lxor 0b0111_1111 )

let binary =
  let rec loop op zruns xruns yruns =
    match xruns, yruns with
      | [], [] ->
          zruns

      | Literal lx :: x_rest , Literal ly :: y_rest ->
          let op_x_y = op lx ly in
          let zr = Literal op_x_y in
          let zruns = add_run zr zruns in
          loop op zruns x_rest y_rest

      | Literal lx :: x_rest , Uniform (bit, length) :: y_rest ->
          let ly = literal_of_bit bit in
          let op_x_y = op lx ly in
          let zr = Literal op_x_y in
          let zruns = add_run zr zruns in
          let length = length - 7 in
          let y_rest =
            if length = 0 then
              (* exhuasted the 'y' uniform *)
              y_rest
            else
              Uniform (bit, length) :: y_rest
          in
          loop op zruns x_rest y_rest

      | Uniform (bit, length) :: x_rest, Literal ly :: y_rest ->
          let lx = literal_of_bit bit in
          let op_x_y = op lx ly in
          let zr = Literal op_x_y in
          let zruns = add_run zr zruns in
          let length = length - 7 in
          let x_rest =
            if length = 0 then
              (* exhuasted the 'x' uniform *)
              x_rest
            else
              Uniform (bit, length) :: x_rest
          in
          loop op zruns x_rest y_rest

      | Uniform (bit_x,length_x) :: x_rest, Uniform (bit_y,length_y) :: y_rest ->
          let z_01 = op (int_of_bool bit_x) (int_of_bool bit_y) in
          let bit_z =
            match z_01 with
              | 0 -> false
              | 1 -> true
              | _ -> assert false
          in

          if length_x = length_y then
            (* uniform runs have identical length; output has same length *)
            let zr = Uniform (bit_z, length_x) in
            let zruns = add_run zr zruns in
            loop op zruns x_rest y_rest

          else if length_x < length_y then
            (* y uniform is longer; conceptually, we split it in two, so
               the first part has the same length as x *)
            let zr = Uniform (bit_z, length_x) in
            let zruns = add_run zr zruns in
            let shorter_ry = Uniform (bit_y, length_y - length_x) in
            let y_rest = shorter_ry :: y_rest in
            loop op zruns x_rest y_rest

          else (
            assert (length_x > length_y);
            (* x uniform is longer than the y uniform; split as in the
               p previous case *)
            let zr = Uniform (bit_z, length_y) in
            let zruns = add_run zr zruns in
            let shorter_rx = Uniform (bit_x, length_x - length_y) in
            let x_rest = shorter_rx :: x_rest in
            loop op zruns x_rest y_rest
          )

      | _ -> assert false
  in

  fun binary_op dx dy ->
    let { runs = x_runs } = dx in
    let { runs = y_runs } = dy in

    if dx.d_length <> dy.d_length then
      raise (Invalid_argument "binary: bitvectors of length");

    let op = f_of_binary_op binary_op in

    let num_runs, runs_rev = loop op (0, []) x_runs y_runs in
    ignore num_runs;

    let trailing_literal =
      if dx.d_length mod 7 = 0 then
        (* no trailing bits *)
        0
      else
        (* have to apply to operator to the trailing literals *)
        op dx.trailing_literal dy.trailing_literal
    in
    { d_length = dx.d_length;
      runs = List.rev runs_rev;
      trailing_literal;
    }

let negate d  =
  (* negate by xor'ing with all-ones *)
  let ones = uniform true d.d_length in
  binary XOR ones d


type binary_iter = {
  bit_index : int ;
  y_offset : int ;

  (* these are static *)
  y : e;
  x : d;
  op : int -> int -> int;
  fu : bit_index:int -> length:int -> unit;
  fl : bit_index:int -> unit;

}

let iter_literal fl ~bit_index ~length ~literal =
  let fini = bit_index + length in
  let rec loop bit_index probe =
    if bit_index < fini then
      let value = literal land probe <> 0 in
      fl ~bit_index ~value;
      loop (bit_index + 1) (probe lsr 1)
  in
  loop bit_index 0b100_0000



let iter_literal_when_true fl bit_index w =
  if w land 0b100_0000 <> 0 then
    fl ~bit_index:(bit_index    );

    if w land 0b010_0000 <> 0 then
      fl ~bit_index:(bit_index + 1);

    if w land 0b001_0000 <> 0 then
      fl ~bit_index:(bit_index + 2);

    if w land 0b000_1000 <> 0 then
      fl ~bit_index:(bit_index + 3);

    if w land 0b000_0100 <> 0 then
      fl ~bit_index:(bit_index + 4);

    if w land 0b000_0010 <> 0 then
      fl ~bit_index:(bit_index + 5);

    if w land 0b000_0001 <> 0 then
      fl ~bit_index:(bit_index + 6)

let iter_trailing_literal_when_true fl
    ~bit_index ~trailing_literal ~num_trailing_bits =
  if num_trailing_bits > 0 && trailing_literal land 0b100_0000 <> 0 then
    fl ~bit_index:(bit_index + 0);

  if num_trailing_bits > 1 && trailing_literal land 0b010_0000 <> 0 then
    fl ~bit_index:(bit_index + 1);

  if num_trailing_bits > 2 && trailing_literal land 0b001_0000 <> 0 then
    fl ~bit_index:(bit_index + 2);

  if num_trailing_bits > 3 && trailing_literal land 0b000_1000 <> 0 then
    fl ~bit_index:(bit_index + 3);

  if num_trailing_bits > 4 && trailing_literal land 0b000_0100 <> 0 then
    fl ~bit_index:(bit_index + 4);

  if num_trailing_bits > 5 && trailing_literal land 0b000_0010 <> 0 then
    fl ~bit_index:(bit_index + 5)

  (* we can not have a bit with bit index (bit_index+6); otherwise,
     this would not be a trailing literal *)

let binary_de_iter_true_rev =

  let run_of_literal w =
    if w = 0 then
      Uniform (false, 7)
    else if w = 0b0111_1111 then
      Uniform (true, 7)
    else
      Literal w
  in

  let apply iter ~new_zrun ~prev_zrun =
    (* the iteration functions, if applied at all, are applied to
       [prev_zrun] *)
    let zrun, bit_index =
      match new_zrun, prev_zrun with
        | _, Literal lz ->
          iter_literal_when_true iter.fl iter.bit_index lz;
          new_zrun, iter.bit_index + 7

        | Literal nlz, Uniform (false, length) ->
          (* don't iterate *)
          new_zrun, iter.bit_index + length

        | Literal nlz, Uniform (true, length) ->
          iter.fu ~bit_index:iter.bit_index ~length;
          new_zrun, iter.bit_index + length

        | Uniform (n_bit, n_length), Uniform (bit, length) ->
          if n_bit = bit then
            (* delay iteration; the Uniform run is growing *)
            Uniform (n_bit, n_length + length), iter.bit_index
          else (
            if bit then
              iter.fu ~bit_index:iter.bit_index ~length;
            new_zrun, iter.bit_index + length
          )
    in
    { iter with bit_index }, zrun
  in

  let trail iter zrun =
    let bit_index =
      match zrun with
        | Uniform (false, length) ->
          iter.bit_index + length

        | Uniform (true, length) ->
          iter.fu ~bit_index:iter.bit_index ~length;
          iter.bit_index + length

        | Literal w ->
          iter_literal_when_true iter.fl iter.bit_index w;
          iter.bit_index + 7

    in

    if iter.y_offset < iter.y.offset + iter.y.num_bytes then (
      (* there must be one more run to read from y, the trailing literal *)
      let num_trailing_bits = iter.x.d_length mod 7 in
      assert ( num_trailing_bits > 0 && num_trailing_bits < 7 );
      assert (iter.x.d_length - bit_index = num_trailing_bits);

      let num_bytes_read, yrun = read_run iter.y_offset iter.y.a in
      let y_offset = iter.y_offset + num_bytes_read in
      assert ( y_offset = iter.y.offset + iter.y.num_bytes );

      match yrun with
        | Literal y_trailing_literal ->
          let z_trailing_literal =
            iter.op iter.x.trailing_literal y_trailing_literal in
          iter_trailing_literal_when_true iter.fl
            ~bit_index ~trailing_literal:z_trailing_literal
            ~num_trailing_bits

        | Uniform _ -> assert false
    )
  in

  let rec next_xy iter xruns zrun =
    match xruns with
      | xrun :: x_rest ->
        next_y iter x_rest zrun xrun

      | [] ->
        trail iter zrun

  and next_y iter xruns zrun xrun =
    let num_bytes_read, yrun = read_run iter.y_offset iter.y.a in
    let iter = { iter with y_offset = iter.y_offset + num_bytes_read } in
    aligned iter xruns zrun (xrun, yrun)

  and next_x iter xruns zrun yrun =
    match xruns with
      | xrun :: x_rest ->
        aligned iter x_rest zrun (xrun, yrun)

      | [] ->
        trail iter zrun

  and aligned iter xruns prev_zrun = function
    | Literal lx, Literal ly ->
      let op_x_y = iter.op lx ly in
      let new_zrun = run_of_literal op_x_y in
      let iter, zrun = apply iter ~new_zrun ~prev_zrun in
      next_xy iter xruns zrun

    | Literal lx, Uniform (bit, length) ->
      let ly = literal_of_bit bit in
      let op_x_y = iter.op lx ly in
      let new_zrun = run_of_literal op_x_y in
      let iter, zrun = apply iter ~new_zrun ~prev_zrun in
      let length = length - 7 in
      if length = 0 then
        (* exhuasted the 'y' uniform *)
        next_xy iter xruns zrun
      else
        next_x iter xruns zrun (Uniform (bit, length))

    | Uniform (bit, length), Literal ly ->
      let lx = literal_of_bit bit in
      let op_x_y = iter.op lx ly in
      let new_zrun = run_of_literal op_x_y in
      let iter, zrun = apply iter ~new_zrun ~prev_zrun in
      let length = length - 7 in
      if length = 0 then
        (* exhuasted the 'x' uniform *)
        next_xy iter xruns zrun
      else
        next_y iter xruns zrun (Uniform (bit, length))

    | Uniform (bit_x,length_x), Uniform (bit_y,length_y) ->
      let z_01 = iter.op (int_of_bool bit_x) (int_of_bool bit_y) in
      let bit_z =
        match z_01 with
          | 0 -> false
          | 1 -> true
          | _ -> assert false
      in

      if length_x = length_y then
        (* uniform runs have identical length; output has same length *)
        let new_zrun = Uniform (bit_z, length_x) in
        let iter, zrun = apply iter ~new_zrun ~prev_zrun in
        next_xy iter xruns zrun

      else if length_x < length_y then
        (* y uniform is longer; conceptually, we split it in two, so
           the first part has the same length as x *)
        let new_zrun = Uniform (bit_z, length_x) in
        let iter, zrun = apply iter ~new_zrun ~prev_zrun in
        let shorter_y = Uniform (bit_y, length_y - length_x) in
        next_x iter xruns zrun shorter_y

      else (
        assert (length_x > length_y);
        (* x uniform is longer than the y uniform; split as in the
           p previous case *)
        let new_zrun = Uniform (bit_z, length_y) in
        let iter, zrun = apply iter ~new_zrun ~prev_zrun in
        let shorter_x = Uniform (bit_x, length_x - length_y) in
        next_y iter xruns zrun shorter_x
      )

  in
  let binary_de_iter_true_rev fu fl binary_op dx ey =
    if dx.d_length <> ey.e_length then
      raise (Invalid_argument "bitvectors of different lengths");

    let op = f_of_binary_op binary_op in

    let iter = {
      bit_index = 0;
      y_offset = ey.offset;
      op;
      x = dx;
      y = ey;
      fu;
      fl;
    } in

    next_xy iter dx.runs (Uniform (false, 0))
  in
  binary_de_iter_true_rev

let iter_d_true =
  let rec loop fu fl bit_index = function
    | Uniform (bit, length) :: rest ->
      if bit then
        fu ~bit_index ~length;
      loop fu fl (bit_index + length) rest

    | Literal w :: rest ->
      iter_literal_when_true fl bit_index w;
      loop fu fl (bit_index + 7) rest

    | [] -> ()
  in
  fun fu fl { d_length; runs ; trailing_literal } ->
    loop fu fl 0 runs;
    let num_trailing_bits = d_length mod 7 in
    let bit_index = d_length - num_trailing_bits in
    iter_trailing_literal_when_true fl ~trailing_literal ~bit_index
      ~num_trailing_bits

let fold_d_true f_uniform f_literal d q0 =
  let q = ref q0 in
  iter_d_true (
      fun ~bit_index ~length ->
        q := f_uniform ~bit_index ~length !q
    ) (
      fun ~bit_index ->
        q := f_literal ~bit_index !q
    ) d;
    !q

let iter_e fu fl { e_length ; a ; offset; num_bytes } =
  let fini = offset + num_bytes in
  let rec loop byte_index max_num_bytes bit_index bits_remaining =
    if byte_index = fini then
      max_num_bytes, bit_index, bits_remaining
    else
      let num_bytes_read, run = read_run byte_index a in
      let byte_index = byte_index + num_bytes_read in
      let max_num_bytes = max_num_bytes + (max_length_of_run run) in

      let bit_index, bits_remaining =
        match run with
          | Uniform (value, length) ->
            fu ~bit_index ~value ~length;
            bit_index + length, bits_remaining - length

          | Literal literal ->
            let length = min bits_remaining 7 in
            iter_literal fl ~bit_index ~literal ~length;
            bit_index + length, bits_remaining - length
      in
      loop byte_index max_num_bytes bit_index bits_remaining

  in
  let max_num_bytes, bit_index, bits_remaining = loop offset 0 0 e_length in
  assert
    begin
      if (num_bytes <= max_num_bytes) &&
         (bit_index = e_length) &&
         bits_remaining = 0
      then
        true
      else (
        Printf.printf "BV7.iter_e: e_length=%d max_num_bytes=%d \
                       e_length=%d bit_index=%d bits_remaining=%d\n%!"
          e_length max_num_bytes e_length bit_index bits_remaining;
        false
      )
    end

let binary_de_fold_true f_uniform f_literal binary_op dx ey q0 =
  let q = ref q0 in
  binary_de_iter_true_rev (
      fun ~bit_index ~length ->
        q := f_uniform ~bit_index ~length !q
    ) (
      fun ~bit_index ->
        q := f_literal ~bit_index !q
    ) binary_op dx ey;
    !q

let binary_de =
  let rec next_xy op xruns y y_offset zruns =
    match xruns with
      | xrun :: x_rest ->
        next_y op x_rest y y_offset zruns xrun

      | [] ->
        trail y_offset y zruns

  and trail y_offset y zruns =
    if y_offset < y.offset + y.num_bytes then
      (* there must be one more run to read from y, the trailing literal *)
      let num_bytes_read, yrun = read_run y_offset y.a in
      let y_offset = y_offset + num_bytes_read in
      assert ( y_offset = y.offset + y.num_bytes );
      match yrun with
        | Literal trailing_literal ->
          trailing_literal, zruns

        | Uniform _ -> assert false
    else
      0 (* ignored *), zruns

  and next_y op xruns y y_offset zruns xrun =
    let num_bytes_read, yrun = read_run y_offset y.a in
    let y_offset = y_offset + num_bytes_read in
    process op xruns y y_offset zruns (xrun, yrun)

  and next_x op xruns y y_offset zruns yrun =
    match xruns with
      | xrun :: x_rest ->
        process op x_rest y y_offset zruns (xrun, yrun)

      | [] ->
        trail y_offset y zruns

  and process op xruns y y_offset zruns = function
    | Literal lx, Literal ly ->

      let op_x_y = op lx ly in
      let zr = Literal op_x_y in
      let zruns = add_run zr zruns in
      next_xy op xruns y y_offset zruns

    | Literal lx, Uniform (bit, length) ->
      let ly = literal_of_bit bit in
      let op_x_y = op lx ly in
      let zr = Literal op_x_y in
      let zruns = add_run zr zruns in
      let length = length - 7 in
      if length = 0 then
        (* exhuasted the 'y' uniform *)
        next_xy op xruns y y_offset zruns
      else
        next_x op xruns y y_offset zruns (Uniform (bit, length))

    | Uniform (bit, length), Literal ly ->
      let lx = literal_of_bit bit in
      let op_x_y = op lx ly in
      let zr = Literal op_x_y in
      let zruns = add_run zr zruns in
      let length = length - 7 in
      if length = 0 then
        (* exhuasted the 'x' uniform *)
        next_xy op xruns y y_offset zruns
      else
        next_y op xruns y y_offset zruns (Uniform (bit, length))

    | Uniform (bit_x,length_x), Uniform (bit_y,length_y) ->
      let z_01 = op (int_of_bool bit_x) (int_of_bool bit_y) in
      let bit_z =
        match z_01 with
          | 0 -> false
          | 1 -> true
          | _ -> assert false
      in

      if length_x = length_y then
        (* uniform runs have identical length; output has same length *)
        let zr = Uniform (bit_z, length_x) in
        let zruns = add_run zr zruns in
        next_xy op xruns y y_offset zruns

      else if length_x < length_y then
        (* y uniform is longer; conceptually, we split it in two, so
           the first part has the same length as x *)
        let zr = Uniform (bit_z, length_x) in
        let zruns = add_run zr zruns in
        let shorter_y = Uniform (bit_y, length_y - length_x) in
        next_x op xruns y y_offset zruns shorter_y

      else (
        assert (length_x > length_y);
        (* x uniform is longer than the y uniform; split as in the
           p previous case *)
        let zr = Uniform (bit_z, length_y) in
        let zruns = add_run zr zruns in
        let shorter_x = Uniform (bit_x, length_x - length_y) in
        next_y op xruns y y_offset zruns shorter_x
      )

  in

  let binary_op_rev binary_op dx ey =
    if dx.d_length <> ey.e_length then
      raise (Invalid_argument "binary: bitvectors of length");

    let op = f_of_binary_op binary_op in

    let y_trailing_literal, (num_runs, runs_rev) =
      next_xy op dx.runs ey ey.offset (0, []) in

    ignore num_runs;

    let trailing_literal =
      if dx.d_length mod 7 = 0 then
        (* no trailing bits *)
        0
      else
        (* have to apply to operator to the trailing literals *)
        op dx.trailing_literal y_trailing_literal
    in
    { d_length = dx.d_length;
      runs = runs_rev; (* reversed! *)
      trailing_literal;
    }
  in

  let binary_op binary_op dx ey =
    let dz_rev = binary_op_rev binary_op dx ey in
    { dz_rev with runs = List.rev dz_rev.runs }

  in
  binary_op

(*  let tt, feature_d  = binary_de AND observation_subset feature_e in
    iter_true tt;
    let ff = binary_dd AND_NOT observation_subset (rev feature_d) in
    (* or *)
    let ff = binary_de AND_NOT observation_subset feature_e in
    iter_true ff;
*)
