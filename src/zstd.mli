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

  val is_closed : t -> bool

  (** [write stream buf off len] compresses [len] bytes from [buf] starting
      at [off]. Compressed output is passed to the [writer] callback. *)
  val write : t -> bytes -> int -> int -> unit

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

  val is_closed : t -> bool

  (** [read stream buf off len] decompresses up to [len] bytes into [buf]
      starting at [off]. Returns 0 at end of stream. *)
  val read : t -> bytes -> int -> int -> int

  (** [close stream] frees the context. Idempotent. *)
  val close : t -> unit
end
