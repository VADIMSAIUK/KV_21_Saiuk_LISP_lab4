(defun selection-sort-hof (lst &key (key #'identity) (test #'<))
  "Selection sort using higher-order functions and key/test parameters."
  (labels
      ((find-min-hof (elements
         (reduce (lambda (a b)
                   (if (funcall test (funcall key a)
                                (funcall key b))
                       a
                       b))
                 elements))

       (remove-first-hof (elem lst)
         (let ((removed nil))
           (remove-if (lambda (x)
                        (if (and (not removed)
                                 (equal x elem))
                            (progn
                              (setf removed t)
                              t)
                            nil))
                      lst))))

    (if (null lst)
        nil
        (let* ((min (find-min-hof lst))
               (rest (remove-first-hof min lst)))
          (cons min (selection-sort-hof rest :key key :test test))))))

(defun check-selection-sort-hof (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-hof input :key key :test test)
                 expected)
          name))

(defun test-selection-sort-hof ()
  (check-selection-sort-hof "test-1" '(3 1 2) '(1 2 3))
  (check-selection-sort-hof "test-2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-selection-sort-hof "test-3" '(1 2 3 4) '(1 2 3 4))
  (check-selection-sort-hof "test-4" '() '())
  ;; test with :key
  (check-selection-sort-hof "test-5"
                            '((3 . a) (1 . b) (2 . c))
                            '((1 . b) (2 . c) (3 . a))
                            :key #'car)
  ;; test with :test >
  (check-selection-sort-hof "test-6"
                            '(1 2 3)
                            '(3 2 1)
                            :test #'>))
