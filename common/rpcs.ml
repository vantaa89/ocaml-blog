open! Core
open! Import

module Tag = struct
  type t =
    { name : string
    ; slug : string
    }
  [@@deriving bin_io, sexp_of]
end

module Tag_with_count = struct
  type t =
    { tag : Tag.t
    ; post_count : int
    }
  [@@deriving bin_io, sexp_of]
end

module Post = struct
  type t =
    { title : string
    ; slug : string
    ; content : string Map.M(Language).t
    ; created_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; tags : Tag.t list
    ; special_post : bool
    ; hidden : bool
    }
  [@@deriving bin_io, sexp_of]
end

module Post_summary = struct
  let max_excerpt_length = 150

  type t =
    { title : string
    ; slug : string
    ; excerpt : string
    ; created_at : Time_ns.Stable.Alternate_sexp.V1.t
    ; tags : Tag.t list
    ; languages : Language.t list
    ; hidden : bool
    }
  [@@deriving bin_io, sexp_of]
end

module Publication = struct
  type t =
    { title : string
    ; image_url : string
    ; authors : string
    ; journal : string
    ; link : string option
    }
  [@@deriving bin_io, sexp_of]
end

module News = struct
  type t =
    { content : string
    ; date : Date.t
    }
  [@@deriving bin_io, sexp_of]
end

module Get_main_page = struct
  module Response = struct
    type t =
      { main_post : Post.t option
      ; recent_posts : Post_summary.t list
      ; publications : Publication.t list
      ; news : News.t list
      }
    [@@deriving bin_io, sexp_of]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-main-page"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Get_about_page = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"get-about-page"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:[%bin_type_class: Post.t option]
  ;;
end

module Get_post = struct
  module Query = struct
    type t = { slug : string } [@@deriving bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-post"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:[%bin_type_class: Post.t option]
  ;;
end

module Get_post_list = struct
  module Query = struct
    type t =
      { tag_slug : string option
      ; limit : int option
      ; offset : int option
      }
    [@@deriving bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-post-list"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:[%bin_type_class: Post_summary.t list]
  ;;
end

module Get_tags = struct
  let rpc =
    Rpc.Rpc.create
      ~name:"get-tags"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:[%bin_type_class: Tag_with_count.t list]
  ;;
end

module Search_posts = struct
  module Query = struct
    type t = { query : string } [@@deriving bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"search-posts"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:[%bin_type_class: Post_summary.t list]
  ;;
end

module Render_markdown = struct
  module Query = struct
    type t = { markdown : string } [@@deriving bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"render-markdown"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:[%bin_type_class: string]
  ;;
end

let websocket_path = "/rpc"
