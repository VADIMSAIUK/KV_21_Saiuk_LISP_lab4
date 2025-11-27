(in-package :cl-user)

(defun add-next-reducer (&key (transform #'identity))
  (lambda (acc element)
    (let ((t-element (funcall transform element)))
      (if (null acc)
          (list (cons t-element nil))
          (let* ((last-pair (car (last acc))))
            (setf (cdr last-pair) t-element)
            (append acc (list (cons t-element nil))))))))

(defun check-reducer (name input-lst expected &key (transform #'identity))
  (let ((result (reduce (add-next-reducer :transform transform)
                        input-lst
                        :initial-value nil
                        :from-end nil)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name)))

(defun test-reducer ()
  (check-reducer "test-1"
                 '(5 6 7 8)
                 '((5 . 6) (6 . 7) (7 . 8) (8 . nil)))

  (check-reducer "test-2"
                 '(1 -2 3)
                 '((-1 . 2) (2 . -3) (-3 . nil))
                 :transform (lambda (x) (* x -1)))

  (check-reducer "test-3"
                 '("hello")
                 '(("hello" . nil)))

  (check-reducer "test-4"
                 '("a" "b" "c")
                 '(("a!" . "b!") ("b!" . "c!") ("c!" . nil))
                 :transform (lambda (s) (concatenate 'string s "!")))

  (check-reducer "test-5"
                 '()
                 '()))

