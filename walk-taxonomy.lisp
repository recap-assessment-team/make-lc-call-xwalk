#!/usr/local/bin/lispscript


(defparameter hmm nil)

(with-a-file "xlation.lisp" :r
  (setq hmm (read stream!)))


(defvar holder nil)
(setq holder (make-hash-table :test #'equal))


(defun add-to-hash (call sub start-str end-str)
  (let ((start      start-str)
        (theend     end-str)
        (subnice    (delim (reverse sub) :sep #\>)))
    (loop for i from start to theend do
          ; (setf (gethash (fn "~A~A" call i) holder) subnice)
          (ft "~A -> ~A~%" (fn "~A~A" call i) subnice))))



(defun terminalp (form)
  (if (integerp (car form))
    (if (null (cdddr form)) t nil)
    (if (null (cddr form)) t nil)))


(defun deeper (form)
  (if (integerp (car form))
    (cdddr form)
    (cddr form)))

(defun get-sub (form)
  (if (integerp (car form))
    (caddr form)
    (cadr form)))

(defun get-call (form)
  (if (integerp (car form))
    (cdddr form)
    (cddr form)))


(defun dd (root &optional (call "") (sub nil))
  (let ((thecar (car root))
        (thecdr (cdr root)))
    (when (null root) nil)
    ; (debug-these root call sub) (ft "~%~%")
    (cond
      ((= (length root) 1)
              (dd (car root)))
      ((terminalp root)
              ; (ft "TERMINAL!~%")
              (add-to-hash call (cons (caddr root) sub) (car root) (cadr root)))
      (t
              (let ((newcall    (fn "~A~A" call (if (integerp (car root)) "" (car root))))
                    (newsub     (cons (get-sub root) sub)))
                (mapcar (lambda (x) (dd x newcall newsub)) (deeper root)))))))


(mapcar #'dd hmm)

