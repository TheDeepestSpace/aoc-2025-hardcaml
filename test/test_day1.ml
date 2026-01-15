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
  while not (Bits.to_bool !(outputs.dial.valid)) do
    cycle ()
  done;
  let dial = Bits.to_unsigned_int !(outputs.dial.value) in
  print_s [%message "Result" (dial : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()

;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)

(* let waves_config =
  Waves_config.to_directory "/tmp/"
|> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;; *)

let waves_config =
  Waves_config.to_directory "/tmp/"
|> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical simple_testbench;
  [%expect {|
    (Result (dial 32))
    Saved waves to /tmp/test_day1_ml_Simple_test__optionally_saving_waveforms_to_disk.vcd
    |}]
;;
