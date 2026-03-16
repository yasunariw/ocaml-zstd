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

  val create : ?level:int -> ?dict:string -> unit -> t

  val is_closed : t -> bool

  (** [write stream ~writer buf off len] compresses [len] bytes from [buf]
      starting at [off]. Compressed output is passed to [writer].

      The [bytes] buffer passed to [writer] is reused across calls; the contents
      are only valid for the duration of the callback. If [writer] raises an
      exception, the stream is automatically closed and the exception is
      re-raised. *)
  val write : t -> writer:(bytes -> int -> int -> unit) ->
    bytes -> int -> int -> unit

  (** [flush stream ~writer] flushes buffered data.
      See {!write} for [writer] buffer and exception semantics. *)
  val flush : t -> writer:(bytes -> int -> int -> unit) -> unit

  (** [close stream ~writer] ends the frame and frees the context.
      Idempotent. See {!write} for [writer] buffer and exception semantics. *)
  val close : t -> writer:(bytes -> int -> int -> unit) -> unit
end

(** {2 Streaming Decompression} *)

module Decompress_stream : sig
  type t

  val in_size : unit -> int
  val out_size : unit -> int

  val create : ?dict:string -> unit -> t

  val is_closed : t -> bool

  (** [read stream ~reader buf off len] decompresses up to [len] bytes into
      [buf] starting at [off]. Calls [reader] to obtain compressed input.
      Returns 0 at end of stream.

      If [reader] raises an exception, the stream is automatically closed and
      the exception is re-raised. *)
  val read : t -> reader:(bytes -> int -> int -> int) ->
    bytes -> int -> int -> int

  (** [close stream] frees the context. Idempotent. *)
  val close : t -> unit
end
