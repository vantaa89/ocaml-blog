open! Core

module type Id = sig
  type t [@@deriving compare, equal, sexp_of]

  (** Raises unless [int] is positive *)
  val of_int_exn : int -> t

  val to_int : t -> int
  val to_pgx_async_value : t -> Pgx_async.Value.t
  val of_pgx_async_value_exn : Pgx_async.Value.t -> t
end

module User_id : Id
module Image_id : Id
module Tag_id : Id
module Post_id : Id
module Publication_id : Id
module News_id : Id
