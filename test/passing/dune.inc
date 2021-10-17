; -------- Test: `conv.ml` --------

; The PPX-dependent executable under test
(executable
 (name conv)
 (modes js) (modules conv)
 (preprocess (pps ppx_deriving_brr))
 (libraries js_of_ocaml brr))

; Run the PPX on the `.ml` file
(rule
 (targets conv.actual)
 (deps
  (:pp pp.exe)
  (:input conv.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_brr)
 (action
  (diff conv.expected conv.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (deps ./conv.bc.js)
 (package ppx_deriving_brr)
 (action
  (run node ./conv.bc.js)))

; -------- Test: `opt.ml` --------

; The PPX-dependent executable under test
(executable
 (name opt)
 (modes js) (modules opt)
 (preprocess (pps ppx_deriving_brr))
 (libraries js_of_ocaml brr))

; Run the PPX on the `.ml` file
(rule
 (targets opt.actual)
 (deps
  (:pp pp.exe)
  (:input opt.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_brr)
 (action
  (diff opt.expected opt.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (deps ./opt.bc.js)
 (package ppx_deriving_brr)
 (action
  (run node ./opt.bc.js)))

; -------- Test: `simple.ml` --------

; The PPX-dependent executable under test
(executable
 (name simple)
 (modes js) (modules simple)
 (preprocess (pps ppx_deriving_brr))
 (libraries js_of_ocaml brr))

; Run the PPX on the `.ml` file
(rule
 (targets simple.actual)
 (deps
  (:pp pp.exe)
  (:input simple.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_brr)
 (action
  (diff simple.expected simple.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (deps ./simple.bc.js)
 (package ppx_deriving_brr)
 (action
  (run node ./simple.bc.js)))


