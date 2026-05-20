(** Zstandard - fast lossless compression algorithm *)

exception Error of string

val version : unit -> (int * int * int)

(** [dict] optional pre-defined dictionary content (see dictBuilder) *)
val compress : level:int -> ?dict:string -> string -> string

(**
  [decompress orig_size ?dict s]

  [orig_size] specifies size of buffer for decompression (not less than original size of uncompressed [s])
  [dict] must be identical to the one used during compression, otherwise uncompressed data will be corrupted.
*)
val decompress : int -> ?dict:string -> string -> string

val get_decompressed_size : string -> int

(** {1 Streaming Interface} *)

(** {2 Streaming Compression} *)

module Compress_stream : sig
  type t

  val in_size : unit -> int
  val out_size : unit -> int

  (** [create ?level ?dict ~writer ()] creates a streaming compressor.

      [writer] receives compressed output chunks. The [bytes] buffer passed to
      [writer] is reused across calls; the contents are only valid for the
      duration of the callback. If [writer] raises an exception, the stream is
      automatically closed and the exception is re-raised. *)
  val create : ?level:int -> ?dict:string ->
    writer:(bytes -> int -> int -> unit) -> unit -> t

  (** [create_bigstring ?level ?dict ~writer ()] is like {!create} but [writer]
      receives compressed output as a {!Bigstringaf.t} slice. The bigstring is
      an internal buffer reused across calls; its contents are only valid for
      the duration of the callback and must not be retained. *)
  val create_bigstring : ?level:int -> ?dict:string ->
    writer:(Bigstringaf.t -> off:int -> len:int -> unit) -> unit -> t

  val is_closed : t -> bool

  (** [write stream buf off len] compresses [len] bytes from [buf] starting
      at [off]. Compressed output is passed to the [writer] callback. *)
  val write : t -> bytes -> int -> int -> unit

  (** [write_bigstring stream buf ~off ~len] compresses [len] bytes from [buf]
      starting at [off]. The bigstring is read directly without an internal
      copy. Compressed output is passed to the [writer] callback. *)
  val write_bigstring : t -> Bigstringaf.t -> off:int -> len:int -> unit

  (** [flush stream] flushes buffered data. *)
  val flush : t -> unit

  (** [close stream] ends the frame and frees the context. Idempotent. *)
  val close : t -> unit
end

(** {2 Streaming Decompression} *)

module Decompress_stream : sig
  type t

  val in_size : unit -> int
  val out_size : unit -> int

  (** [create ?dict ~reader ()] creates a streaming decompressor.

      [reader] is called to obtain compressed input. If [reader] raises an
      exception, the stream is automatically closed and the exception is
      re-raised. *)
  val create : ?dict:string ->
    reader:(bytes -> int -> int -> int) -> unit -> t

  (** [create_bigstring ?dict ~reader ()] is like {!create} but [reader]
      writes directly into a {!Bigstringaf.t}. The bigstring is an internal
      buffer reused across calls; [reader] must only write into it during
      the call and must not retain a reference. *)
  val create_bigstring : ?dict:string ->
    reader:(Bigstringaf.t -> off:int -> len:int -> int) -> unit -> t

  val is_closed : t -> bool

  (** [read stream buf off len] decompresses up to [len] bytes into [buf]
      starting at [off]. Returns 0 at end of stream. *)
  val read : t -> bytes -> int -> int -> int

  (** [read_bigstring stream buf ~off ~len] decompresses up to [len] bytes
      into [buf] starting at [off]. Output is written directly into the
      bigstring without an internal copy. Returns 0 at end of stream. *)
  val read_bigstring : t -> Bigstringaf.t -> off:int -> len:int -> int

  (** [close stream] frees the context. Idempotent. *)
  val close : t -> unit
end
