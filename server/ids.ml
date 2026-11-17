open! Core

module type Id = sig
  type t [@@deriving compare, equal, sexp_of]

  val of_int_exn : int -> t
  val to_int : t -> int
  val to_pgx_async_value : t -> Pgx_async.Value.t
  val of_pgx_async_value_exn : Pgx_async.Value.t -> t
end

module Id = struct
  type t = int [@@deriving compare, equal, sexp_of]

  let of_int_exn num =
    match Int.sign num with
    | Pos -> num
    | Zero | Neg -> raise_s [%message "Id should be positive" (num : int)]
  ;;

  let to_int = Fn.id
  let to_pgx_async_value t = to_int t |> Pgx_async.Value.of_int
  let of_pgx_async_value_exn value = Pgx_async.Value.to_int_exn value |> of_int_exn
end

module User_id = Id
module Image_id = Id
module Tag_id = Id
module Post_id = Id
module Publication_id = Id
module News_id = Id
