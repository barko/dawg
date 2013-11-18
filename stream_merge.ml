module Make( Elem : Heap.ORDERED ) = struct

  module IndexedElem = struct
    exception Done

    type t = Elem.t Stream.t

    let leq stream1 stream2 =
      match Stream.peek stream1, Stream.peek stream2 with
        | Some head1, Some head2 -> Elem.leq head1 head2
        | None, Some head2 -> true
        | Some head1, None -> false
        | _ -> assert false

  end

  module H = Heap.Make( IndexedElem )

  let create streams =
    let heap = List.fold_left (
      fun heap stream ->
        H.insert stream heap
    ) H.empty streams in

    let heap = ref heap in

    let rec next_opt i =
      try
        let stream = H.find_min !heap in
        heap := H.delete_min !heap;
        let value = Stream.next stream in
        heap := H.insert stream !heap;
        Some value
      with
        | Stream.Failure -> next_opt i
        | H.Empty -> None

    in

    Stream.from next_opt


end
