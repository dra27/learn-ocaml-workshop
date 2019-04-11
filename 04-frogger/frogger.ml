open Base
open Scaffold

[@@@warning "-27-32"]

module Frog = struct
  type t =
    { position : Position.t
    } [@@deriving fields]

  let create = Fields.create
end

module World = struct
  type t =
    { frog  : Frog.t
    } [@@deriving fields]

  let create = Fields.create
end

let create_frog () =
  let position =
    Position.create
      ~x:(Scaffold.Board.num_cols / 2)
      ~y:0
  in
  Frog.create ~position
;;

let create () =
  World.create
    ~frog:(create_frog ())
;;

let tick (world : World.t) =
  world
;;

let handle_input (world : World.t) key =
  let new_pos =
    let old_pos = world.frog.position in
    match key with
    | Key.Arrow_up ->
      { old_pos with y = old_pos.y + 1 }
    | Key.Arrow_down ->
      { old_pos with y = old_pos.y - 1 }
    | Key.Arrow_left ->
      { old_pos with x = old_pos.x - 1 }
    | Key.Arrow_right ->
      { old_pos with x = old_pos.x + 1 }
  in
  World.create ~frog:(Frog.create ~position:new_pos)
;;

let draw (world : World.t) =
  [Image.frog_up, world.frog.position]
;;

let handle_event world (event : Event.t) =
  match event with
  | Tick -> tick world
  | Keypress k -> handle_input world k
;;

let finished world =
  false
;;
