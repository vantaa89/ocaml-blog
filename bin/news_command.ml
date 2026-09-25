open! Core
open! Async
open! Blog_server

let add_news_command =
  Command.async_or_error
    ~summary:"Add a news item"
    (let%map_open.Command content =
       flag "-content" (required string) ~doc:"TEXT what the news says"
     and date =
       flag
         "-date"
         (required date)
         ~doc:"YYYY-MM-DD the date of the news (default: today)"
     in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let%map.Deferred.Or_error news = Database.News.create db ~content ~date in
         print_s [%message "Created news" ~id:(news.id : int)]))
;;

let list_news_command =
  Command.async_or_error
    ~summary:"List every news item, newest first"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let%map.Deferred.Or_error news = Database.News.list db in
         List.iter news ~f:(fun (news : Database_schema.News.t) ->
           print_s
             [%message
               ""
                 ~id:(news.id : int)
                 ~date:(news.date : Date.t)
                 ~content:(news.content : string)])))
;;

let delete_news_command =
  Command.async_or_error
    ~summary:"Delete a news item"
    (let%map_open.Command id =
       flag "-id" (required int) ~doc:"ID the news to delete, as [list] shows"
     in
     fun () -> Database.with_connection ~f:(fun db -> Database.News.delete db ~id))
;;

let command =
  Command.group
    ~summary:"Manage the news shown on the front page"
    [ "add", add_news_command; "list", list_news_command; "delete", delete_news_command ]
;;
