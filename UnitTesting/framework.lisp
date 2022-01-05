
(defvar *testname* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ~A ... ~a~%" result *testname* form)
  result)

(defmacro check (&body forms)
  `(combine-results 
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro with-gensyms (syms &body body)
    `(let ,(loop for s in syms collect `(,s (gensym)))
           ,@body))

(defmacro combine-results (&body forms)
    (with-gensyms (result)
                  `(let ((,result t))
                     ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
                     ,result)))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*testname* (append *testname* (list  ',name))))
       ,@body)))

(deftest test-+ ()
  (check (= (+ 1 2) 3)
         (= (+ 1 2 3) 6)
         (= (+ -1 -3) -4 )))

(deftest test* ()
  (check (= (* 1 1) 1)
         (= (* 4 5) 20)))

(deftest run-all-tests ()
  (combine-results 
    (test-+)
    (test*)))
