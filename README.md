<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Лабораторна робота №4</b><br/>
"Функції вищого порядку та замикання"<br/> 
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">
    <strong>Студент</strong>: <em><strong>Саюк Вадим Анатолійович</strong></em>
</p>
<p align="right">
    <strong>Група</strong>: <em><strong>КВ-21</strong></em>
</p>
<p align="right">
    <strong>Рік</strong>: <em><strong>2025</strong></em>
</p>
## Загальне завдання
    Завдання складається з двох частин:
    1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
        - використати функції вищого порядку для роботи з послідовностями 
        - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test 
    2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом

## Варіант першої частини №1(16 варіант за модулем 5)
Алгоритм сортування вибором за незменшенням.

## Лістинг коду (частина №1):
```lisp
(defun selection-sort-hof (lst &key (key #'identity) (test #'<))
  "Selection sort using higher-order functions and key/test parameters."
  (labels
      ((find-min-hof (elements)
         ;; reduce to find element with minimal key
         (reduce (lambda (a b)
                   (if (funcall test (funcall key a)
                                (funcall key b))
                       a
                       b))
                 elements))

       (remove-first-hof (elem lst)
         ;; remove only the first occurrence of elem
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
```

### Перевірка тестів (частина №1):
```lisp
(defun check-selection-sort-hof (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (selection-sort-hof input :key key :test test)
                 expected)
          name))
```
### Тестові набори (частина №1):
```lisp
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
```
### Результат тестів (частина №1):
```lisp
CL-USER> (test-selection-sort-hof)
passed... test-1
passed... test-2
passed... test-3
passed... test-4
passed... test-5
passed... test-6
NIL
CL-USER> 
```
## Варіант другої частини №4:
Написати функцію add-next-reducer , яка має один ключовий параметр — функціюtransform . add-next-reducer має повернути функцію, яка при застосуванні в якостіпершого аргументу reduce робить наступне: кожен елемент списку-аргументу reduce перетворюється на точкову пару, де в комірці CAR знаходиться значення поточногоелемента, а в комірці CDR знаходиться значення наступного елемента списку (тобтотого, що знаходиться "справа"). Якщо функція transform передана, тоді значення поточного і наступного елементів, що потраплять у результат, мають бути змінені згідноtransform . Обмеження, які накладаються на використання функції-результату next-reducer при передачі у add reduce визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції reduce from-end та разів.

## Лістинг коду для (частина №2):
```lisp
(in-package :cl-user)

(defun add-next-reducer (&key (transform #'identity))
  (lambda (acc element)
    (let ((t-element (funcall transform element)))
      (if (null acc)
          (list (cons t-element nil))
          (let* ((last-pair (car (last acc))))
            (setf (cdr last-pair) t-element)
            (append acc (list (cons t-element nil))))))))
```
### Перевірка тестів (частина №2):
```lisp
(defun check-reducer (name input-lst expected &key (transform #'identity))
  (let ((result (reduce (add-next-reducer :transform transform)
                        input-lst
                        :initial-value nil
                        :from-end nil)))
    (format t "~:[FAILED~;passed~]... ~a~%"
            (equal result expected)
            name)))

```
### Тестові набори (чатина №2):
```lisp
(defun test-reducer ()
  ;; послідовні натуральні числа
  (check-reducer "test-1"
                 '(5 6 7 8)
                 '((5 . 6) (6 . 7) (7 . 8) (8 . nil)))

  ;; transform: множення на -1 (зміна знаку)
  (check-reducer "test-2"
                 '(1 -2 3)
                 '((-1 . 2) (2 . -3) (-3 . nil))
                 :transform (lambda (x) (* x -1)))

  ;; один елемент зі строкою
  (check-reducer "test-3"
                 '("hello")
                 '(("hello" . nil)))

  ;; список рядків, transform: додати "!"
  (check-reducer "test-4"
                 '("a" "b" "c")
                 '(("a!" . "b!") ("b!" . "c!") ("c!" . nil))
                 :transform (lambda (s) (concatenate 'string s "!")))

  ;; порожній список
  (check-reducer "test-5"
                 '()
                 '()))

```
### Результат тестів (частниа №2):
```lisp
passed... test-1
passed... test-2
passed... test-3
passed... test-4
passed... test-5
NIL
```



