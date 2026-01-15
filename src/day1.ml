open! Core
open! Hardcaml
open! Signal
open! Always

let ascii_width = 8
let code_width  = 16

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a

    (* simple AXI-style stream for ASCII data from the input *)
    ; _ascii_char       : 'a [@bits ascii_width]
    ; _ascii_char_valid : 'a
    ; _ascii_char_last  : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { code : 'a With_valid.t [@bits code_width]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Init
    | Read_direction
    | Read_digit
    | Read_newline
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create _scope ({ clock; clear; _ascii_char; _ascii_char_valid; _ascii_char_last} : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let _sm = State_machine.create (module States) spec in
  let hard_val = Signal.of_int_trunc ~width:code_width 42 in
    { code = { value = hard_val; valid = vdd } }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O)
  in
    Scoped.hierarchical ~scope ~name:"day1" create

