type entry =
  { path : string
  ; content : string
  ; mimetype : string
  ; id : string option
  ; properties : string list }
type metadata =
  { identifier : string
  ; title : string
  ; last_modified : string
  ; date : string option
  ; author : string option }

val create : string -> metadata -> entry list -> string list -> unit
(** [create filename metadata entries spine] *)

val epub_datetime_of_time : Core.Time_float.t -> string
