(in-package :cl-braces.compiler.examples)

(defun example-expressions ()
  (let* ((source-code "
          -(3 + 3 + ( 5 + 4 ) * 8)
        ")
         (code (compiler:compile-this source-code)))
    (time (machine:execute code))))


(defun example-variables-and-conditionals ()
  (let* ((source-code "
        var x int = 3;
        var y, z = 4, 5;

        if a := x; a > 3 {
            x
        }
        ")
         (code (compiler:compile-this source-code)))
    (time (machine:execute code))))
