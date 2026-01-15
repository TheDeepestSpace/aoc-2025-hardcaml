open! Core
open! Hardcaml
open! Signal
open! Always

let ascii_width = 8
let dial_width  = 16
let dial_start  = 50
let max_dial    = 99

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
    ; code             : 'a With_valid.t [@bits dial_width]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Init
    | Read_direction
    | Read_digit
    | Update_dial_and_code
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; ascii_char; ascii_char_valid; ascii_char_last} : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) spec in
  let%hw_var dir  = Variable.reg spec ~width:1 in
  let%hw_var num  = Variable.reg spec ~width:dial_width in
  let%hw_var dial = Variable.reg spec ~width:dial_width in
  let%hw_var code = Variable.reg spec ~width:dial_width in
  let%hw_var last = Variable.reg spec ~width:1 in
  let is_last = last.value ==: vdd in
  let ten = Signal.of_int_trunc ~width:(width num.value) 10 in
  let ascii_char_to_dir =
    let is_l = ascii_char ==:. Char.to_int 'L' in
    mux2 is_l gnd vdd
  in
  let ascii_char_to_digit =
    let is_digit = (ascii_char >=:. Char.to_int '0') &: (ascii_char <=:. Char.to_int '9') in
    let digit = uresize ~width:dial_width (ascii_char -:. Char.to_int '0') in
    mux2 is_digit digit (zero dial_width)
  in
  let ascii_char_is_newline = ascii_char ==:. Char.to_int '\n' in
  let turn_left = (dir.value ==: gnd) in
  let max_dial_plus_one = Signal.of_int_trunc ~width:dial_width (max_dial + 1) in
  let left_adjusted_dial =
    mux2 (num.value >: dial.value)
      (max_dial_plus_one -: (num.value -: dial.value))
      (dial.value -: num.value)
  in
  let right_adjusted_dial =
    let updated_dial = dial.value +: num.value in
    mux2 (updated_dial >:. max_dial)
      (updated_dial -: max_dial_plus_one)
      updated_dial
  in
  compile
    [ sm.switch
        [ ( Init,
          [ sm.set_next Read_direction
          ; num  <-- zero dial_width
          ; dial <-- Signal.of_int_trunc ~width:dial_width dial_start
          ; code <-- zero dial_width
          ; last <-- gnd
          ]
          )
        ; ( Read_direction,
          [ if_ ascii_char_valid
              [ sm.set_next Read_digit
              ; dir <-- ascii_char_to_dir
              ; num <-- zero dial_width
              ]
              [ sm.set_next Read_direction ]
          ]
          )
        ; ( Read_digit,
          [ if_ ascii_char_valid
              [ if_ ascii_char_is_newline
                  [ sm.set_next Update_dial_and_code ]
                  [ sm.set_next Read_digit
                  ; let prod       = num.value *: ten               in
                    let prod_trunc = uresize ~width:dial_width prod in
                    num <-- prod_trunc +: ascii_char_to_digit
                  ]
              ; last <-- ascii_char_last
              ]
              [ sm.set_next Read_digit ]
          ]
          )
        ; ( Update_dial_and_code,
            let next_dial =
              mux2 turn_left
                left_adjusted_dial
                right_adjusted_dial
            in
            [ dial <-- next_dial
            ; when_ (next_dial ==:. 0)
                [ code <-- code.value +:. 1 ]
            ; if_ is_last
                [ sm.set_next Done ]
                [ sm.set_next Read_direction ]
            ]
          )
        ; ( Done, [ sm.set_next Done ] )
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

