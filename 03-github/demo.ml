#use "topfind";;
#thread;;
#require "github.unix";;

let stream = Github.Issue.for_repo ~user:"ocaml" ~repo:"opam" ();;
let read n s =
  let open Github.Monad in
  let rec read a n s =
    Github.Stream.next s
    >>= function
    | Some (issue, next) when n > 0 ->
      read (issue::a) (pred n) next
    | _ ->
      return (List.rev a)
  in
  read [] n s;;

read 10 stream |> Github.Monad.run |> Lwt_main.run;;

let program =
  let open Lwt.Infix in
  Github_cookie_jar.init ()
  >>= fun jar ->
    Github_cookie_jar.get jar ~name:"reasonconf"
  >>= function
  | None -> Lwt.fail (Failure "invalid token!")
  | Some auth ->
    let token = Github.Token.of_auth auth in
    let stream = Github.Issue.for_repo ~token ~user:"ocaml" ~repo:"opam" ()
    in
    read 10 stream |> Github.Monad.run
in
Lwt_main.run program;;
