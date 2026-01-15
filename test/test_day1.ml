open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day1 = Advent_of_fpga.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )
let sample_input =
  let root = Option.value (Sys.getenv "DUNE_SOURCEROOT") ~default:"." in
  In_channel.read_all (Filename.concat root "test/input1.txt")

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one ASCII byte *)
  let feed_ascii ?(last = false) n =
    inputs.ascii_char <--. n;
    inputs.ascii_char_valid := Bits.vdd;
    inputs.ascii_char_last := if last then Bits.vdd else Bits.gnd;
    cycle ();
    inputs.ascii_char_valid := Bits.gnd;
    inputs.ascii_char_last := Bits.gnd;
    cycle ()
  in
  let feed_string s =
    let last_index = String.length s - 1 in
    String.iteri s ~f:(fun i ch ->
      feed_ascii ~last:(i = last_index) (Char.to_int ch))
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  feed_string sample_input;
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.code.valid)) do
    cycle ()
  done;
  let code = Bits.to_unsigned_int !(outputs.code.value) in
  print_s [%message "Result" (code : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()

;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
let waves_config = Waves_config.no_waves

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical simple_testbench;
  [%expect {| (Result (code 3)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.compile (Re.Posix.re "day1.*"))
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect {|
    (Result (code 3))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │day1$o$ascii_char_ready     ││────────────────────────────────────────────────────────────│
    │                            ││                                                            │
    │day1$o$code$valid           ││────────────────────────────────────────────────────────────│
    │                            ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day1$o$code$value           ││ 3                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
