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

val cstream_in_size : unit -> int
val cstream_out_size : unit -> int
val dstream_in_size : unit -> int
val dstream_out_size : unit -> int

(** {2 Streaming Compression} *)

type compress_stream

val compress_stream_create : ?level:int -> ?dict:string -> unit -> compress_stream

val compress_stream_is_closed : compress_stream -> bool

(** [compress_stream_write stream ~writer buf off len] compresses [len] bytes
    from [buf] starting at [off]. Compressed output is passed to [writer].

    The [bytes] buffer passed to [writer] is reused across calls; the contents
    are only valid for the duration of the callback. If [writer] raises an
    exception, the stream enters an undefined state and should be closed
    without further use. *)
val compress_stream_write :
  compress_stream -> writer:(bytes -> int -> int -> unit) ->
  bytes -> int -> int -> unit

(** [compress_stream_flush stream ~writer] flushes buffered data.
    See {!compress_stream_write} for [writer] buffer and exception semantics. *)
val compress_stream_flush :
  compress_stream -> writer:(bytes -> int -> int -> unit) -> unit

(** [compress_stream_close stream ~writer] ends the frame and frees the context.
    Idempotent. See {!compress_stream_write} for [writer] buffer and exception
    semantics. *)
val compress_stream_close :
  compress_stream -> writer:(bytes -> int -> int -> unit) -> unit
