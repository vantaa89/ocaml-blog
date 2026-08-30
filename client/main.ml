open! Core
open! Import

let () =
  Async_js.init ();
  Start.start App.component
;;
