open! Core
open! Import

(** [Time_ns.Stable.Alternate_sexp.V1] does not export [equal], which Bonsai's [Model]
    signature requires of every type stored in client-side state. *)
module Time_ns_alternate_sexp = struct
  include Time_ns.Stable.Alternate_sexp.V1

  let equal = Time_ns.equal
end

module Tag = struct
  type t =
    { name : string
    ; slug : string
    }
  [@@deriving bin_io, sexp, equal]
end

module Tag_with_count = struct
  type t =
    { tag : Tag.t
    ; post_count : int
    }
  [@@deriving bin_io, sexp, equal]
end

module Post = struct
  type t =
    { title : string
    ; slug : string
    ; content : string Map.M(Language).t
    ; created_at : Time_ns_alternate_sexp.t
    ; tags : Tag.t list
    ; special_post : bool
    ; hidden : bool
    }
  [@@deriving bin_io, sexp, equal]
end

module Post_summary = struct
  let max_excerpt_length = 150

  type t =
    { title : string
    ; slug : string
    ; excerpt : string
    ; created_at : Time_ns_alternate_sexp.t
    ; tags : Tag.t list
    ; languages : Language.t list
    }
  [@@deriving bin_io, sexp, equal]
end

module Publication = struct
  type t =
    { title : string
    ; image_url : string
    ; authors : string
    ; journal : string
    ; link : string option
    }
  [@@deriving bin_io, sexp, equal]
end

module News = struct
  type t =
    { content : string
    ; date : Date.t
    }
  [@@deriving bin_io, sexp, equal]
end

module Get_main_page = struct
  module Response = struct
    type t =
      { main_post : Post.t option
      ; recent_posts : Post_summary.t list
      ; publications : Publication.t list
      ; news : News.t list
      }
    [@@deriving bin_io, sexp, equal]
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
  module Response = struct
    type t = Post.t option [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-about-page"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Get_post = struct
  module Query = struct
    type t = { slug : string } [@@deriving bin_io, sexp, equal]
  end

  module Response = struct
    type t = Post.t option [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-post"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Get_post_list = struct
  module Query = struct
    type t =
      { tag_slug : string option
      ; limit : int option
      ; offset : int option
      }
    [@@deriving bin_io, sexp, equal]
  end

  module Response = struct
    type t = Post_summary.t list [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-post-list"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Get_tags = struct
  module Response = struct
    type t = Tag_with_count.t list [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-tags"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Search_posts = struct
  module Query = struct
    type t = { query : string } [@@deriving bin_io, sexp, equal]
  end

  module Response = struct
    type t = Post_summary.t list [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"search-posts"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Set_post_hidden = struct
  module Query = struct
    type t =
      { slug : string
      ; hidden : bool
      }
    [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"set-post-hidden"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Unit.bin_t
  ;;
end

module Get_current_user = struct
  module Response = struct
    type t =
      | Logged_in of { username : string }
      | Not_logged_in
    [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-current-user"
      ~version:0
      ~bin_query:Unit.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Render_markdown = struct
  module Query = struct
    type t = { markdown : string } [@@deriving bin_io, sexp, equal]
  end

  module Response = struct
    type t = string [@@deriving bin_io, sexp, equal]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"render-markdown"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
