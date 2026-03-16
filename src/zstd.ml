open Zstd_stubs

module Size_t = Unsigned.Size_t
module F = C.Functions
module T = C.Types

exception Error of string

let version () =
  let n = F.version_number () in
  (n / 10_000, (n / 100) mod 100, n mod 100)

let bracket res destroy k =
  let r = try k res with exn -> let () = destroy res in raise exn in
  let () = destroy res in
  r

let check r = if F.is_error r then raise (Error (F.get_error_name r))

let free_cctx x = check (F.free_cctx x)
let free_dctx x = check (F.free_dctx x)

let compress ~level ?dict s =
  let open Ctypes in
  let len = Size_t.of_int (String.length s) in
  let dst_size = F.compress_bound len in
  let dst = allocate_n char ~count:(Size_t.to_int dst_size) in
  let r =
    match dict with
    | None -> F.compress (to_voidp dst) dst_size s len level
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (F.create_cctx ()) free_cctx begin fun cctx ->
        F.compress_using_dict cctx (to_voidp dst) dst_size s len dict dlen level
      end
  in
  check r;
  string_from_ptr dst ~length:(Size_t.to_int r)

let decompress orig ?dict s =
  let open Ctypes in
  let dst = allocate_n char ~count:orig in
  let r =
    match dict with
    | None -> F.decompress (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int (String.length s))
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (F.create_dctx ()) free_dctx begin fun dctx ->
        F.decompress_using_dict dctx (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int (String.length s)) dict dlen
      end
  in
  check r;
  string_from_ptr dst ~length:(Size_t.to_int r)

let get_decompressed_size s =
  let r = F.get_frame_content_size s (Size_t.of_int (String.length s)) in
  if r = T.content_size_error then
    raise (Error "content size error")
  else if r = T.content_size_unknown then
    raise (Error "content size unknown")
  else
    Unsigned.ULLong.to_int r

(* Streaming API *)

let seti s f i = Ctypes.setf s f (Size_t.of_int i)
let geti s f = Size_t.to_int (Ctypes.getf s f)

type bigstring = Bigstringaf.t

let bigstring_create n = Bigstringaf.create n

let bigstring_start ba = Ctypes.bigarray_start Ctypes.array1 ba

let cstream_in_size () = Size_t.to_int (F.cstream_in_size ())
let cstream_out_size () = Size_t.to_int (F.cstream_out_size ())
let dstream_in_size () = Size_t.to_int (F.dstream_in_size ())
let dstream_out_size () = Size_t.to_int (F.dstream_out_size ())

type compress_stream = {
  cctx: [`CCtx] Ctypes.structure Ctypes.ptr;
  in_buf: bigstring;
  in_size: int;
  out_buf: bigstring;
  out_size: int;
  out_bytes: bytes;
  zstd_in: [`InBuffer] Ctypes.structure;
  zstd_out: [`OutBuffer] Ctypes.structure;
  mutable closed: bool;
  mutable freed: bool;
}

let compress_stream_create ?level ?dict () =
  let open Ctypes in
  let cctx = F.create_cctx () in
  if is_null cctx then raise (Error "ZSTD_createCCtx failed");
  try
    (match level with
     | Some l -> check (F.cctx_set_parameter cctx T.c_compressionLevel l)
     | None -> ());
    (match dict with
     | Some d -> check (F.cctx_load_dictionary cctx d (Size_t.of_int (String.length d)))
     | None -> ());
    let in_size = cstream_in_size () in
    let out_size = cstream_out_size () in
    let in_buf = bigstring_create in_size in
    let out_buf = bigstring_create out_size in
    let out_bytes = Bytes.create out_size in
    let zstd_in = make F.in_buffer in
    let zstd_out = make F.out_buffer in
    setf zstd_in F.in_buffer_src (to_voidp (bigstring_start in_buf));
    setf zstd_out F.out_buffer_dst (to_voidp (bigstring_start out_buf));
    let s = { cctx; in_buf; in_size; out_buf; out_size; out_bytes;
              zstd_in; zstd_out; closed = false; freed = false } in
    Gc.finalise (fun s ->
      if not s.freed then begin
        s.freed <- true;
        ignore (F.free_cctx s.cctx)
      end) s;
    s
  with exn ->
    ignore (F.free_cctx cctx);
    raise exn

let compress_stream_is_closed s = s.closed

let drain_output s ~writer directive =
  let open Ctypes in
  seti s.zstd_in F.in_buffer_size 0;
  seti s.zstd_in F.in_buffer_pos 0;
  let rec loop () =
    seti s.zstd_out F.out_buffer_size s.out_size;
    seti s.zstd_out F.out_buffer_pos 0;
    let remaining = F.compress_stream2 s.cctx (addr s.zstd_out) (addr s.zstd_in) directive in
    check remaining;
    let out_pos = geti s.zstd_out F.out_buffer_pos in
    if out_pos > 0 then begin
      Bigstringaf.blit_to_bytes s.out_buf ~src_off:0 s.out_bytes ~dst_off:0 ~len:out_pos;
      writer s.out_bytes 0 out_pos
    end;
    if Size_t.to_int remaining > 0 then loop ()
  in
  loop ()

let compress_stream_write s ~writer buf off len =
  if s.closed then raise (Error "stream is closed");
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Zstd.compress_stream_write";
  if len = 0 then ()
  else begin
    let open Ctypes in
    let pos = ref 0 in
    while !pos < len do
      let chunk = min s.in_size (len - !pos) in
      Bigstringaf.blit_from_bytes buf ~src_off:(off + !pos) s.in_buf ~dst_off:0 ~len:chunk;
      seti s.zstd_in F.in_buffer_size chunk;
      seti s.zstd_in F.in_buffer_pos 0;
      while geti s.zstd_in F.in_buffer_pos < chunk do
        seti s.zstd_out F.out_buffer_size s.out_size;
        seti s.zstd_out F.out_buffer_pos 0;
        check (F.compress_stream2 s.cctx (addr s.zstd_out) (addr s.zstd_in) T.e_continue);
        let out_pos = geti s.zstd_out F.out_buffer_pos in
        if out_pos > 0 then begin
          Bigstringaf.blit_to_bytes s.out_buf ~src_off:0 s.out_bytes ~dst_off:0 ~len:out_pos;
          writer s.out_bytes 0 out_pos
        end
      done;
      pos := !pos + chunk
    done
  end

let compress_stream_flush s ~writer =
  if s.closed then raise (Error "stream is closed");
  drain_output s ~writer T.e_flush

let compress_stream_close s ~writer =
  if s.closed then ()
  else begin
    s.closed <- true;
    Fun.protect
      ~finally:(fun () ->
        if not s.freed then begin
          s.freed <- true;
          ignore (F.free_cctx s.cctx)
        end)
      (fun () -> drain_output s ~writer T.e_end)
  end

(* Streaming decompression *)

type decompress_stream = {
  dctx: [`DCtx] Ctypes.structure Ctypes.ptr;
  in_buf: bigstring;
  in_size: int;
  in_bytes: bytes;
  out_buf: bigstring;
  out_size: int;
  zstd_in: [`InBuffer] Ctypes.structure;
  zstd_out: [`OutBuffer] Ctypes.structure;
  mutable in_filled: int;
  mutable in_consumed: int;
  mutable out_pos: int;
  mutable out_avail: int;
  mutable eof: bool;
  mutable last_ret: int;
  mutable closed: bool;
  mutable freed: bool;
}

let decompress_stream_create ?dict () =
  let open Ctypes in
  let dctx = F.create_dctx () in
  if is_null dctx then raise (Error "ZSTD_createDCtx failed");
  try
    (match dict with
     | Some d -> check (F.dctx_load_dictionary dctx d (Size_t.of_int (String.length d)))
     | None -> ());
    let in_size = dstream_in_size () in
    let out_size = dstream_out_size () in
    let in_buf = bigstring_create in_size in
    let in_bytes = Bytes.create in_size in
    let out_buf = bigstring_create out_size in
    let zstd_in = make F.in_buffer in
    let zstd_out = make F.out_buffer in
    setf zstd_in F.in_buffer_src (to_voidp (bigstring_start in_buf));
    setf zstd_out F.out_buffer_dst (to_voidp (bigstring_start out_buf));
    let s = { dctx; in_buf; in_size; in_bytes; out_buf; out_size;
              zstd_in; zstd_out; in_filled = 0; in_consumed = 0;
              out_pos = 0; out_avail = 0;
              eof = false; last_ret = 0; closed = false; freed = false } in
    Gc.finalise (fun s ->
      if not s.freed then begin
        s.freed <- true;
        ignore (F.free_dctx s.dctx)
      end) s;
    s
  with exn ->
    ignore (F.free_dctx dctx);
    raise exn

let decompress_stream_is_closed s = s.closed

let decompress_stream_read s ~reader buf off len =
  if s.closed then raise (Error "stream is closed");
  if off < 0 || len < 0 || off > Bytes.length buf - len then
    invalid_arg "Zstd.decompress_stream_read";
  if len = 0 then 0
  else begin
    let open Ctypes in
    let total = ref 0 in
    let continue = ref true in
    while !total < len && !continue do
      (* drain leftover from internal buffer *)
      if s.out_avail > 0 then begin
        let n = min s.out_avail (len - !total) in
        Bigstringaf.blit_to_bytes s.out_buf ~src_off:s.out_pos buf ~dst_off:(off + !total) ~len:n;
        s.out_pos <- s.out_pos + n;
        s.out_avail <- s.out_avail - n;
        total := !total + n
      end else begin
        (* refill input buffer if exhausted *)
        if s.in_consumed >= s.in_filled && not s.eof then begin
          let n = reader s.in_bytes 0 s.in_size in
          if n < 0 || n > s.in_size then
            invalid_arg "Zstd.decompress_stream_read: reader returned invalid length";
          if n = 0 then
            s.eof <- true
          else begin
            Bigstringaf.blit_from_bytes s.in_bytes ~src_off:0 s.in_buf ~dst_off:0 ~len:n;
            s.in_filled <- n;
            s.in_consumed <- 0
          end
        end;
        (* detect clean EOF vs truncation *)
        if s.in_consumed >= s.in_filled && s.eof then begin
          if s.last_ret <> 0 then
            raise (Error "truncated compressed data");
          continue := false
        end else begin
          (* decompress one step with full output buffer *)
          let consumed_before = s.in_consumed in
          seti s.zstd_in F.in_buffer_size s.in_filled;
          seti s.zstd_in F.in_buffer_pos s.in_consumed;
          seti s.zstd_out F.out_buffer_size s.out_size;
          seti s.zstd_out F.out_buffer_pos 0;
          let ret = F.decompress_stream s.dctx (addr s.zstd_out) (addr s.zstd_in) in
          check ret;
          s.in_consumed <- geti s.zstd_in F.in_buffer_pos;
          let produced = geti s.zstd_out F.out_buffer_pos in
          s.last_ret <- Size_t.to_int ret;
          if produced > 0 then begin
            let n = min produced (len - !total) in
            Bigstringaf.blit_to_bytes s.out_buf ~src_off:0 buf ~dst_off:(off + !total) ~len:n;
            total := !total + n;
            if produced > n then begin
              s.out_pos <- n;
              s.out_avail <- produced - n
            end
          end else if s.in_consumed = consumed_before && s.last_ret <> 0 then
            raise (Error "decompression made no progress")
        end
      end
    done;
    !total
  end

let decompress_stream_close s =
  if s.closed then ()
  else begin
    s.closed <- true;
    if not s.freed then begin
      s.freed <- true;
      ignore (F.free_dctx s.dctx)
    end
  end
