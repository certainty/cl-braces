(in-package :tests.vm.bytecode)

;; Disabled since the result of chunks isn't deterministic
;;
;; (define-test disassembler-works ()
;;   (snapshots:assert-snapshot-equals
;;    "disassembler.snapshot"
;;    (with-output-to-string (s) (bytecode:disass (compiler:compile "3 + 3") :stream s)))
;;   )
