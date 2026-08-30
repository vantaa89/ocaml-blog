open! Core
open! Import

let where_to_connect : Rpc_effect.Where_to_connect.t = Url Rpcs.websocket_path
let retry_interval = Time_ns.Span.of_sec 5.
