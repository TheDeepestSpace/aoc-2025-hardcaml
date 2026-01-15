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
    ; ascii_char       : 'a [@bits ascii_width]
    ; ascii_char_valid : 'a
    ; ascii_char_last  : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ascii_char_ready : 'a
    ; code             : 'a With_valid.t [@bits code_width]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Init
    | Read_direction
    | Read_digit
    | Update_code
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; ascii_char; ascii_char_valid; ascii_char_last} : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in
  let%hw_var dir  = Variable.reg spec ~width:1 in
  let%hw_var num  = Variable.reg spec ~width:code_width in
  let%hw_var code = Variable.reg spec ~width:code_width in
  let%hw_var last = Variable.reg spec ~width:1 in
  let is_last = last.value ==: vdd in
  let ten = Signal.of_int_trunc ~width:(width num.value) 10 in
  let ascii_char_to_dir =
    let is_l = ascii_char ==:. Char.to_int 'L' in
    mux2 is_l gnd vdd
  in
  let ascii_char_to_digit =
    let is_digit = (ascii_char >=:. Char.to_int '0') &: (ascii_char <=:. Char.to_int '9') in
    let digit = uresize ~width:code_width (ascii_char -:. Char.to_int '0') in
    mux2 is_digit digit (zero code_width)
  in
  let ascii_char_is_newline = ascii_char ==:. Char.to_int '\n' in
  let turn_left = (dir.value ==: gnd) in
  compile
    [ sm.switch
        [ ( Init,
          [ sm.set_next Read_direction
          ; num  <-- zero code_width
          ; code <-- zero code_width
          ; last <-- gnd
          ]
          )
        ; ( Read_direction,
          [ if_ ascii_char_valid
              [ sm.set_next Read_digit
              ; dir <-- ascii_char_to_dir
              ]
              [ sm.set_next Read_direction ]
          ]
          )
        ; ( Read_digit,
          [ if_ ascii_char_valid
              [ if_ ascii_char_is_newline
                  [ sm.set_next Update_code ]
                  [ sm.set_next Read_digit
                  ; let prod       = num.value *: ten               in
                    let prod_trunc = uresize ~width:code_width prod in
                    num <-- prod_trunc +: ascii_char_to_digit
                  ]
              ; last <-- ascii_char_last
              ]
              [ sm.set_next Read_digit ]
          ]
          )
        ; ( Update_code,
          [ if_ turn_left
              [ code <-- code.value +: num.value]
              [ code <-- code.value -: num.value]
          ; if_ is_last
              [ sm.set_next Read_direction ]
              [ sm.set_next Done ]
          ]
          )
        ; ( Done,
          [ sm.set_next Done ]
          )
        ]
    ];
    { code =
      { value = code.value
      ; valid = (sm.is Done)
      }
    ; ascii_char_ready = (sm.is Read_direction) |: (sm.is Read_digit)
    }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O)
  in
    Scoped.hierarchical ~scope ~name:"day1" create

