open! Core
open! Import

(** Returns a hash string that includes the information of the algorithm, parameters, and
   salt. Therefore, it suffices to store only this function's result in the DB. *)
val hash_exn : string -> string

(** Returns [false] on a wrong password, and an error when [password_hash] is malformed.  *)
val verify : password_hash:string -> password:string -> bool Or_error.t
