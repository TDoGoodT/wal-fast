(lang dune 3.0)

(name wal_fast)

(generate_opam_files true)

(source
 (github TDoGoodT/wal-fast))

(license "BSD-3-Clause")

(authors "Snir Bachar")

(maintainers "Snir Bachar")

(package
 (name wal_fast)
 (synopsis "Blazing fast Waveform Analysis Language engine for huge EDA simulations")
 (description
  "WAL-FAST is a high-performance reimplementation of the Waveform Analysis Language (WAL) in OCaml. Designed for multi-gigabyte VCD files from hardware simulators. Based on WAL by Miro Haller, ICS-JKU Linz (BSD-3-Clause).")
 (depends
  (ocaml (>= 5.0.0))
  (cmdliner (>= 1.2.0))
  (alcotest :with-test)))
