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

(** {2 Streaming Decompression} *)

type decompress_stream

val decompress_stream_create : ?dict:string -> unit -> decompress_stream

val decompress_stream_is_closed : decompress_stream -> bool

(** [decompress_stream_read stream ~reader buf off len] decompresses up to [len]
    bytes into [buf] starting at [off]. Calls [reader] to obtain compressed input.
    Returns 0 at end of stream.

    If [reader] raises an exception, the stream enters an undefined state and
    should be closed without further use. *)
val decompress_stream_read :
  decompress_stream -> reader:(bytes -> int -> int -> int) ->
  bytes -> int -> int -> int

(** [decompress_stream_close stream] frees the context. Idempotent. *)
val decompress_stream_close : decompress_stream -> unit

(** {2 Channel Interface}

    Higher-level wrapper around the streaming API providing a channel-like
    interface with simple read/write operations. *)

type out_channel
type in_channel

(** [open_out ?level ?dict writer] creates a compressing output channel.
    See {!compress_stream_write} for [writer] semantics. *)
val open_out : ?level:int -> ?dict:string -> (bytes -> int -> int -> unit) -> out_channel

(** [output ch buf off len] compresses [len] bytes from [buf] starting at [off]. *)
val output : out_channel -> bytes -> int -> int -> unit

(** [output_char ch c] compresses a single character. *)
val output_char : out_channel -> char -> unit

(** [flush ch] flushes buffered compressed data without ending the frame. *)
val flush : out_channel -> unit

(** [close_out ch] ends the frame, flushes remaining data, and frees resources.
    Idempotent. *)
val close_out : out_channel -> unit

(** [open_in ?dict reader] creates a decompressing input channel.
    See {!decompress_stream_read} for [reader] semantics. *)
val open_in : ?dict:string -> (bytes -> int -> int -> int) -> in_channel

(** [input ch buf off len] decompresses up to [len] bytes into [buf] starting
    at [off]. Returns the number of bytes actually read, or [0] at end of
    stream. *)
val input : in_channel -> bytes -> int -> int -> int

(** [input_char ch] reads and decompresses a single character.
    @raise End_of_file at end of stream *)
val input_char : in_channel -> char

(** [close_in ch] frees resources. Idempotent. *)
val close_in : in_channel -> unit
