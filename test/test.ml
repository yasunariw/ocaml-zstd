open ExtLib
open Printf

let test (name,src) =
  let orig = String.length src in
  let a = Array.init 20 (fun level -> Zstd.compress ~level src) in
  a |> Array.iteri begin fun i s ->
    assert (Zstd.get_decompressed_size s = orig);
    let s = Zstd.decompress orig s in
    if s <> src then failwith @@ sprintf "%s : level %d failed" name i;
  end;
  let best = Array.fold_left (fun m s -> min m (String.length s)) (String.length a.(0)) a in
  let best_level = Array.findi (fun s -> String.length s = best) a in
  printf "%50s : best compression %02.1fx at level %d : %d -> %d\n" name (float orig /. float best) best_level orig best

let stream_compress ?level ?dict src =
  let buf = Buffer.create 256 in
  let writer b off len = Buffer.add_subbytes buf b off len in
  let s = Zstd.Compress_stream.create ?level ?dict ~writer () in
  Zstd.Compress_stream.write s (Bytes.of_string src) 0 (String.length src);
  Zstd.Compress_stream.close s;
  Buffer.contents buf

let stream_decompress_reader ?dict ?(buf_size=Zstd.Decompress_stream.out_size ()) ~reader () =
  let s = Zstd.Decompress_stream.create ~reader ?dict () in
  let out = Buffer.create 256 in
  let tmp = Bytes.create buf_size in
  let rec loop () =
    let n = Zstd.Decompress_stream.read s tmp 0 (Bytes.length tmp) in
    if n > 0 then begin
      Buffer.add_subbytes out tmp 0 n;
      loop ()
    end
  in
  loop ();
  Zstd.Decompress_stream.close s;
  Buffer.contents out

let string_reader src =
  let pos = ref 0 in
  fun b off len ->
    let n = min len (String.length src - !pos) in
    Bytes.blit_string src !pos b off n;
    pos := !pos + n;
    n

let stream_decompress ?dict ?buf_size src =
  stream_decompress_reader ?dict ?buf_size ~reader:(string_reader src) ()

let test_streaming () =
  let data =
    let buf = Buffer.create 8192 in
    for i = 0 to 199 do
      Buffer.add_string buf (sprintf "line %d: the quick brown fox jumps over the lazy dog\n" i);
      if i mod 10 = 0 then Buffer.add_string buf (String.make 50 (Char.chr (65 + i mod 26)));
    done;
    Buffer.contents buf
  in
  let buf_writer () =
    let buf = Buffer.create 256 in
    buf, (fun b off len -> Buffer.add_subbytes buf b off len)
  in
  let expect_exn f =
    (try ignore (f ()); assert false
     with _ -> ())
  in
  (* 1. Round-trip: stream compress -> stream decompress *)
  let () =
    let compressed = stream_compress data in
    let decompressed = stream_decompress compressed in
    assert (decompressed = data);
    printf "  1. round-trip: OK\n"
  in
  (* 2. Simple compress -> stream decompress *)
  let () =
    let compressed = Zstd.compress ~level:3 data in
    let decompressed = stream_decompress compressed in
    assert (decompressed = data);
    printf "  2. simple-to-stream: OK\n"
  in
  (* 3. Stream compress -> simple decompress (using known original size) *)
  let () =
    let compressed = stream_compress ~level:3 data in
    let decompressed = Zstd.decompress (String.length data) compressed in
    assert (decompressed = data);
    printf "  3. stream-to-simple: OK\n"
  in
  (* 4. Incremental writes *)
  let () =
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    let src = Bytes.of_string data in
    for i = 0 to String.length data - 1 do
      Zstd.Compress_stream.write s src i 1
    done;
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress (Buffer.contents buf) in
    assert (decompressed = data);
    printf "  4. incremental writes: OK\n"
  in
  (* 5. Empty stream *)
  let () =
    assert (stream_decompress (stream_compress "") = "");
    printf "  5. empty stream: OK\n"
  in
  (* 6. Flush produces output before close; full round-trip with flush+close *)
  let () =
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    Zstd.Compress_stream.write s (Bytes.of_string data) 0 (String.length data);
    Zstd.Compress_stream.flush s;
    assert (Buffer.length buf > 0);
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress (Buffer.contents buf) in
    assert (decompressed = data);
    printf "  6. flush: OK\n"
  in
  (* 7. Dictionary *)
  let () =
    let dict = String.init 1000 (fun i -> Char.chr (i mod 256)) in
    let compressed = stream_compress ~dict data in
    let decompressed = stream_decompress ~dict compressed in
    assert (decompressed = data);
    printf "  7. dictionary: OK\n"
  in
  (* 8. Truncated stream *)
  let () =
    let compressed = stream_compress data in
    let half = String.sub compressed 0 (String.length compressed / 2) in
    (try
      ignore (stream_decompress half);
      assert false
    with Zstd.Error _ ->
      printf "  8. truncated stream: OK\n")
  in
  (* 9. Concatenated frames *)
  let () =
    let part1 = "first frame data" in
    let part2 = "second frame data" in
    let c1 = stream_compress part1 in
    let c2 = stream_compress part2 in
    let decompressed = stream_decompress (c1 ^ c2) in
    assert (decompressed = part1 ^ part2);
    printf "  9. concatenated frames: OK\n"
  in
  (* 10. Large data (10MB+) *)
  let () =
    let size = 10 * 1024 * 1024 + 37 in
    let src = String.init size (fun i ->
      if i mod 512 < 384 then Char.chr ((i * 7 + i / 256) mod 256)
      else Char.chr (i / 4096 mod 26 + 65))
    in
    let compressed = stream_compress ~level:1 src in
    let decompressed = stream_decompress compressed in
    assert (decompressed = src);
    printf " 10. large data (10MB+): OK\n"
  in
  (* 11. Input validation *)
  let () =
    let in_size = Zstd.Decompress_stream.in_size () in
    let tmp = Bytes.create 64 in
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> -1) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp 0 64));
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> in_size + 1) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp 0 64));
    expect_exn (fun () ->
      let ds = Zstd.Decompress_stream.create ~reader:(fun _ _ _ -> 0) () in
      Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () ->
        Zstd.Decompress_stream.read ds tmp max_int 1));
    expect_exn (fun () ->
      let noop _ _ _ = () in
      let s = Zstd.Compress_stream.create ~writer:noop () in
      Fun.protect ~finally:(fun () -> Zstd.Compress_stream.close s) (fun () ->
        Zstd.Compress_stream.write s tmp max_int 1));
    printf " 11. input validation: OK\n"
  in
  (* 12. Byte-at-a-time decompression (exercises internal buffering) *)
  let () =
    let compressed = stream_compress data in
    assert (stream_decompress ~buf_size:1 compressed = data);
    printf " 12. byte-at-a-time decompression: OK\n"
  in
  (* 13. GC stress (exercises GC safety of ctypes structs) *)
  let () =
    let gc_churn () =
      ignore (Sys.opaque_identity (Array.init 100 (fun i -> String.make 64 (Char.chr (i mod 256)))));
      Gc.compact ()
    in
    (* compress in 512B chunks with GC churn between each *)
    let buf, writer = buf_writer () in
    let s = Zstd.Compress_stream.create ~level:1 ~writer () in
    let src = Bytes.of_string data in
    let chunk = 512 in
    let pos = ref 0 in
    while !pos < String.length data do
      let len = min chunk (String.length data - !pos) in
      Zstd.Compress_stream.write s src !pos len;
      gc_churn ();
      pos := !pos + len
    done;
    Zstd.Compress_stream.close s;
    (* decompress with small buf_size: many read+decompress calls *)
    let compressed = Buffer.contents buf in
    let reader =
      let r = string_reader compressed in
      fun b off len -> let n = r b off len in gc_churn (); n
    in
    assert (stream_decompress_reader ~buf_size:512 ~reader () = data);
    printf " 13. GC stress: OK\n"
  in
  (* Bigstring helpers reused by tests 14, 16, 19, 21. *)
  let bs_buf_writer () =
    let buf = Buffer.create 256 in
    buf, (fun ba ~off ~len -> Buffer.add_string buf (Bigstringaf.substring ba ~off ~len))
  in
  let bs_string_reader src =
    let pos = ref 0 in
    fun ba ~off ~len ->
      let n = min len (String.length src - !pos) in
      Bigstringaf.blit_from_string src ~src_off:!pos ba ~dst_off:off ~len:n;
      pos := !pos + n;
      n
  in
  let stream_decompress_bs ?(buf_size=Zstd.Decompress_stream.out_size ()) src =
    let ds = Zstd.Decompress_stream.create_bigstring ~reader:(bs_string_reader src) () in
    let out = Buffer.create 256 in
    let tmp = Bigstringaf.create buf_size in
    let rec loop () =
      let n = Zstd.Decompress_stream.read_bigstring ds tmp ~off:0 ~len:buf_size in
      if n > 0 then begin
        Buffer.add_string out (Bigstringaf.substring tmp ~off:0 ~len:n);
        loop ()
      end
    in
    loop ();
    Zstd.Decompress_stream.close ds;
    Buffer.contents out
  in
  (* 14. Bigstring round-trip *)
  let () =
    let buf, writer = bs_buf_writer () in
    let s = Zstd.Compress_stream.create_bigstring ~writer () in
    let src_ba = Bigstringaf.of_string ~off:0 ~len:(String.length data) data in
    Zstd.Compress_stream.write_bigstring s src_ba ~off:0 ~len:(String.length data);
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress_bs (Buffer.contents buf) in
    assert (decompressed = data);
    printf " 14. bigstring round-trip: OK\n"
  in
  (* 15. Mixed sinks on Compress: write + write_bigstring on a bigstring-sink stream *)
  let () =
    let buf, writer = bs_buf_writer () in
    let s = Zstd.Compress_stream.create_bigstring ~level:1 ~writer () in
    let data_ba = Bigstringaf.of_string ~off:0 ~len:(String.length data) data in
    let data_b = Bytes.of_string data in
    let n = String.length data in
    let half = n / 2 in
    Zstd.Compress_stream.write s data_b 0 half;
    Zstd.Compress_stream.write_bigstring s data_ba ~off:half ~len:(n - half);
    Zstd.Compress_stream.close s;
    let decompressed = stream_decompress (Buffer.contents buf) in
    assert (decompressed = data);
    printf " 15. mixed sinks on Compress: OK\n"
  in
  (* 16. Mixed sinks on Decompress: read + read_bigstring on a bigstring-source stream *)
  let () =
    let compressed = stream_compress data in
    let ds = Zstd.Decompress_stream.create_bigstring ~reader:(bs_string_reader compressed) () in
    let out = Buffer.create 256 in
    let tmp_b = Bytes.create 100 in
    let tmp_ba = Bigstringaf.create 100 in
    let alt = ref false in
    let continue = ref true in
    while !continue do
      let n =
        if !alt then Zstd.Decompress_stream.read ds tmp_b 0 100
        else Zstd.Decompress_stream.read_bigstring ds tmp_ba ~off:0 ~len:100
      in
      if n = 0 then continue := false
      else begin
        if !alt then Buffer.add_subbytes out tmp_b 0 n
        else Buffer.add_string out (Bigstringaf.substring tmp_ba ~off:0 ~len:n);
        alt := not !alt
      end
    done;
    Zstd.Decompress_stream.close ds;
    assert (Buffer.contents out = data);
    printf " 16. mixed sinks on Decompress: OK\n"
  in
  (* 17. write_bigstring offset: ensure only the [off..off+len) slice is compressed *)
  let () =
    let payload = "Hello, zstd bigstring world!" in
    let pad = 50 in
    let total = pad + String.length payload + pad in
    let ba = Bigstringaf.create total in
    for i = 0 to total - 1 do Bigstringaf.set ba i '\xff' done;
    Bigstringaf.blit_from_string payload ~src_off:0 ba ~dst_off:pad ~len:(String.length payload);
    let buf, writer = bs_buf_writer () in
    let s = Zstd.Compress_stream.create_bigstring ~writer () in
    Zstd.Compress_stream.write_bigstring s ba ~off:pad ~len:(String.length payload);
    Zstd.Compress_stream.close s;
    assert (stream_decompress (Buffer.contents buf) = payload);
    printf " 17. write_bigstring offset: OK\n"
  in
  (* 18. read_bigstring offset: write into middle of larger bigstring, sentinel head/tail *)
  let () =
    let payload = "Hello, zstd bigstring world!" in
    let compressed = stream_compress payload in
    let pad = 50 in
    let total = pad + String.length payload + pad in
    let ba = Bigstringaf.create total in
    for i = 0 to total - 1 do Bigstringaf.set ba i 'Z' done;
    let ds = Zstd.Decompress_stream.create ~reader:(string_reader compressed) () in
    let got = ref 0 in
    let continue = ref true in
    while !continue do
      let want = String.length payload - !got in
      if want = 0 then continue := false
      else begin
        let n = Zstd.Decompress_stream.read_bigstring ds ba ~off:(pad + !got) ~len:want in
        if n = 0 then continue := false else got := !got + n
      end
    done;
    Zstd.Decompress_stream.close ds;
    assert (!got = String.length payload);
    for i = 0 to pad - 1 do assert (Bigstringaf.get ba i = 'Z') done;
    for i = pad + String.length payload to total - 1 do assert (Bigstringaf.get ba i = 'Z') done;
    assert (Bigstringaf.substring ba ~off:pad ~len:(String.length payload) = payload);
    printf " 18. read_bigstring offset: OK\n"
  in
  (* 19. Byte-at-a-time bigstring (stresses src/dst restoration between tiny chunks) *)
  let () =
    let buf, writer = bs_buf_writer () in
    let s = Zstd.Compress_stream.create_bigstring ~level:1 ~writer () in
    let src_ba = Bigstringaf.of_string ~off:0 ~len:(String.length data) data in
    for i = 0 to String.length data - 1 do
      Zstd.Compress_stream.write_bigstring s src_ba ~off:i ~len:1
    done;
    Zstd.Compress_stream.close s;
    assert (stream_decompress_bs ~buf_size:1 (Buffer.contents buf) = data);
    printf " 19. byte-at-a-time bigstring: OK\n"
  in
  (* 20. Bigstring input validation *)
  let () =
    let in_size = Zstd.Decompress_stream.in_size () in
    let ba = Bigstringaf.create 64 in
    let noop_writer _ ~off:_ ~len:_ = () in
    let with_compress f =
      expect_exn (fun () ->
        let s = Zstd.Compress_stream.create_bigstring ~writer:noop_writer () in
        Fun.protect ~finally:(fun () -> Zstd.Compress_stream.close s) (fun () -> f s))
    in
    with_compress (fun s -> Zstd.Compress_stream.write_bigstring s ba ~off:(-1) ~len:1);
    with_compress (fun s -> Zstd.Compress_stream.write_bigstring s ba ~off:0 ~len:(-1));
    with_compress (fun s -> Zstd.Compress_stream.write_bigstring s ba ~off:max_int ~len:1);
    let with_decompress reader f =
      expect_exn (fun () ->
        let ds = Zstd.Decompress_stream.create_bigstring ~reader () in
        Fun.protect ~finally:(fun () -> Zstd.Decompress_stream.close ds) (fun () -> f ds))
    in
    let ok_reader _ ~off:_ ~len:_ = 0 in
    let bad_neg _ ~off:_ ~len:_ = -1 in
    let bad_big _ ~off:_ ~len:_ = in_size + 1 in
    with_decompress ok_reader (fun ds ->
      Zstd.Decompress_stream.read_bigstring ds ba ~off:max_int ~len:1);
    with_decompress bad_neg (fun ds ->
      Zstd.Decompress_stream.read_bigstring ds ba ~off:0 ~len:64);
    with_decompress bad_big (fun ds ->
      Zstd.Decompress_stream.read_bigstring ds ba ~off:0 ~len:64);
    printf " 20. bigstring input validation: OK\n"
  in
  (* 21. Bigstring GC stress (verifies src/dst manipulation survives Gc.compact) *)
  let () =
    let gc_churn () =
      ignore (Sys.opaque_identity (Array.init 100 (fun i -> String.make 64 (Char.chr (i mod 256)))));
      Gc.compact ()
    in
    let buf, writer = bs_buf_writer () in
    let s = Zstd.Compress_stream.create_bigstring ~level:1 ~writer () in
    let src_ba = Bigstringaf.of_string ~off:0 ~len:(String.length data) data in
    let chunk = 512 in
    let pos = ref 0 in
    while !pos < String.length data do
      let len = min chunk (String.length data - !pos) in
      Zstd.Compress_stream.write_bigstring s src_ba ~off:!pos ~len;
      gc_churn ();
      pos := !pos + len
    done;
    Zstd.Compress_stream.close s;
    let compressed = Buffer.contents buf in
    let reader =
      let r = bs_string_reader compressed in
      fun ba ~off ~len -> let n = r ba ~off ~len in gc_churn (); n
    in
    let ds = Zstd.Decompress_stream.create_bigstring ~reader () in
    let out = Buffer.create 256 in
    let tmp = Bigstringaf.create 512 in
    let continue = ref true in
    while !continue do
      let n = Zstd.Decompress_stream.read_bigstring ds tmp ~off:0 ~len:512 in
      if n = 0 then continue := false
      else Buffer.add_string out (Bigstringaf.substring tmp ~off:0 ~len:n)
    done;
    Zstd.Decompress_stream.close ds;
    assert (Buffer.contents out = data);
    printf " 21. bigstring GC stress: OK\n"
  in
  printf "All streaming tests passed.\n"

let () =
  let file f = try Some (f, Std.input_file f) with _ -> None in
  let inputs = [
    file "/bin/bash";
    file Sys.executable_name;
    file "/etc/ld.so.cache";
    file "/etc/mailcap";
    Some ("environment", String.concat " " @@ Array.to_list @@ Unix.environment ());
  ] |> List.filter_map (fun x -> x)
  in
  List.iter test inputs;
  printf "\nStreaming tests:\n";
  test_streaming ()
