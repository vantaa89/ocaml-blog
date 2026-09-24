open! Core
open! Async_kernel
open! Import

val zone : Time_float.Zone.t
val format_time : Time_ns.t -> string

(** An estimated reading time, ["N min read"]. *)
val read_time : string -> string

(** [Rpcs.Post.content] maps each language to its markdown; English is the default. *)
val primary_content : string Map.M(Language).t -> string option

(** Number of posts shown per page. *)
val page_size : int

val paginate : 'a list -> page:int -> 'a list

(** An [<a>] that navigates within the app instead of reloading the page. It still carries
    a real [href], so copying the link or opening it in a new tab behaves normally. *)
val link : ?attrs:Vdom.Attr.t list -> Route.t -> Vdom.Node.t list -> Vdom.Node.t

val tag_node : string -> Vdom.Node.t

val post_card : Rpcs.Post_summary.t -> Vdom.Node.t

val not_found_node : Vdom.Node.t

(** Renders [f response] once the rpc has succeeded, and a placeholder until then. *)
val of_poll
  :  ('query, 'response) Rpc_effect.Poll_result.t
  -> f:('response -> Vdom.Node.t)
  -> Vdom.Node.t

val paginator : route:Route.t -> page:int -> num_pages:int -> Vdom.Node.t
(** Runs KaTeX and highlight.js over the page once the given html has been painted. *)
val rerender_on_change : string Value.t -> unit Computation.t
