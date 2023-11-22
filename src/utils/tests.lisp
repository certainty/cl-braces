(in-package :cl-braces.tests.snapshots)

(defparameter *snapshot-dir* nil)

(defun print-snapshot (value &optional (stream *standard-output*))
  (format stream "~A" value))

(defun assert-snapshot-equals (snapshot-path actual)
  "Asserts that the actual value matches the snapshot. If the snapshot does not exist, it is created."
  (unless *snapshot-dir*
    (error "Snapshot directory not set. Use (setf *snapshot-dir*) to set it."))

  (let ((snapshot-file-name (merge-pathnames snapshot-path *snapshot-dir*)))
    (if (probe-file snapshot-file-name)
        (let ((snapshot (uiop:read-file-string snapshot-file-name))
              (actual (with-output-to-string (stream) (print-snapshot actual stream))))
          (assert-string= snapshot actual))
        (progn
          (format t "Creating snapshot ~a~%" snapshot-file-name)
          (with-open-file (stream snapshot-file-name
                                  :direction :output
                                  :if-exists :supersede)
            (print-snapshot actual stream))))))
