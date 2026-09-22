open! Core
open! Import

let%expect_test "[first_image] reads only [![](url)], up to the first [)]" =
  let print_first_image markdown =
    print_s [%sexp (Utils.first_image ~markdown : string option)]
  in
  print_first_image "Intro ![](/media/a.png) then ![](/media/b.png)";
  [%expect {| (/media/a.png) |}];
  print_first_image "(see ![](/media/inner.png))";
  [%expect {| (/media/inner.png) |}];
  print_first_image "![alt](/media/alt.png) ![](/media/ spaced.png)";
  [%expect {| () |}]
;;
