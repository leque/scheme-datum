;;; scheme-datum.el --- write an elisp datum as a R7RS Scheme datum  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  OOHASHI Daichi

;; Author: OOHASHI Daichi <dico.leque.comicron@gmail.com>
;; Keywords: lisp
;; Package-Requires: (cl-lib)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; scheme-datum.el
;; ===============

;;   Write an elisp datum as a R7RS Scheme datum.

;;   This package can handle

;;   - integers,
;;   - floats,
;;   - strings,
;;   - symbols,
;;   - cons cells,
;;   - nil, and
;;   - vectors.


;; `t' and `nil'
;; ~~~~~~~~~~~~~

;;   `t' is written as a symbol. `nil' is written as an empty list.


;; Examples
;; ~~~~~~~~

;;   ,----
;;   | (require 'scheme-datum)
;;   | 
;;   | (scheme-datum-write-simple '(42 'foo ["bar"] . 3.14))
;;   | ;; -| (42 'foo #("bar") . 3.14)
;;   | 
;;   | (scheme-datum-write-simple (list (/ 1 0.0) (/ -1 0.0) (/ 0 0.0)))
;;   | ;; -| (+inf.0 -inf.0 +nan.0)
;;   | 
;;   | (scheme-datum-write-simple '\(*\ 1\ 2\))
;;   | ;; -| |(* 1 2)|
;;   | 
;;   | (scheme-datum-write-simple '+-*/_~!@$%^&=:<>{})
;;   | ;; -| |+-*/_~!@$%^&=:<>{}|
;;   | 
;;   | (scheme-datum-write-simple '|)
;;   | ;; -| |\||
;;   | 
;;   | (scheme-datum-write-simple '##)
;;   | ;; -| ||
;;   | 
;;   | (scheme-datum-write-simple (list t nil))
;;   | ;; -| (t ())
;;   `----

;;; Code:

(cl-defstruct scheme-datum-true)

(cl-defstruct scheme-datum-false)

(cl-defstruct scheme-datum-nil)

(defconst scheme-datum-true (make-scheme-datum-true)
  "the value written as Scheme's true (#t).")

(defconst scheme-datum-false (make-scheme-datum-false)
  "the value written as Scheme's false (#f)")

(defconst scheme-datum-nil-symbol (make-scheme-datum-nil)
  "the value written as Scheme's nil symbol")

(defun scheme-datum--write-list (obj stream)
  (pcase obj
    (`()
     (princ "()" stream))
    (`(quote ,x)
     (princ "'" stream)
     (scheme-datum-write-simple x stream))
    (`(,'\` ,x)
     (princ "`" stream)
     (scheme-datum-write-simple x stream))
    (`(,'\, ,x)
     (princ "," stream)
     (scheme-datum-write-simple x stream))
    (`(,'\,@ ,x)
     (princ ",@" stream)
     (scheme-datum-write-simple x stream))
    (_
     (princ "(" stream)
     (while obj
       (let ((a (car obj))
             (d (cdr obj)))
         (scheme-datum-write-simple a stream)
         (cond ((null d)
                (setq obj d))
               ((consp d)
                (princ " " stream)
                (setq obj d))
               (t
                (princ " . " stream)
                (scheme-datum-write-simple d stream)
                (setq obj nil)))))
     (princ ")" stream))))

(defun scheme-datum--write-vector (vec stream)
  (princ "#(" stream)
  (cl-loop for elem across vec
           for first = t then nil
           do (progn
                (princ (if first "" " ") stream)
                (scheme-datum-write-simple elem stream)))
  (princ ")" stream))

(defun scheme-datum--write-string (str stream quote escape)
  (princ (format "%c" quote) stream)
  (cl-loop for c across str
           do (princ (cond
                      ((eql c quote)
                       (format "%c%c" escape quote))
                      ((eql c escape)
                       (format "%c%c" escape escape))
                      ((eql c ?\a)
                       "\\a")
                      ((eql c ?\b)
                       "\\b")
                      ((eql c ?\t)
                       "\\t")
                      ((eql c ?\n)
                       "\\n")
                      ((eql c ?\r)
                       "\\r")
                      ((eq (get-char-code-property c 'general-category) 'Cc)
                       (format "\\x%02x;" c))
                      (t
                       (format "%c" c)))
                     stream))
  (princ (format "%c" quote) stream))

(defun scheme-datum--simple-symbol? (sym)
  (let* ((name (symbol-name sym))
         (len (length name)))
    (and (> len 0)
         (cl-loop for sym in '("+inf.0" "-inf.0"
                               "+nan.0" "-nan.0"
                               "+i" "-i"
                               )
                  never (eq t (compare-strings sym nil nil
                                               name nil nil
                                               t)))
         (or
          ;; initial subsequent*
          (and (scheme-datum--initial? (aref name 0))
               (scheme-datum--all-subsequent? name 1))
          (and (scheme-datum--explicit-sign? (aref name 0))
               (or
                ;; explicit-sign
                (= len 1)
                ;; explicit-sign sign-subsequent subsequent*
                (and (scheme-datum--sign-subsequent? (aref name 1))
                     (scheme-datum--all-subsequent? name 2))
                ;; explicit-sign '.' dot-subsequent subsequent*
                (and (>= len 3)
                     (eql (aref name 1) ?.)
                     (scheme-datum--dot-subsequent? (aref name 2))
                     (scheme-datum--all-subsequent? name 3))))
          ;; '.' dot-subsequent subsequent*
          (and (eql (aref name 0) ?.)
               (>= len 2)
               (scheme-datum--dot-subsequent? (aref name 1))
               (scheme-datum--all-subsequent? name 2))))))

(defun scheme-datum--all-subsequent? (str start)
  (cl-loop for i from start below (length str)
           always (scheme-datum--subsequent? (aref str i))))

(defun scheme-datum--initial? (ch)
  (or (scheme-datum--letter? ch)
      (scheme-datum--special-initial? ch)))

(defun scheme-datum--letter? (ch)
  (or (<= ?a ch ?z)
      (<= ?A ch ?Z)))

(defun scheme-datum--digit? (ch)
  (<= ?0 ch ?9))

(defun scheme-datum--special-initial? (ch)
  (cl-loop for c across "!$%&*/:<=>?^_~"
           thereis (eql c ch)))

(defun scheme-datum--explicit-sign? (ch)
  (or (eql ch ?+)
      (eql ch ?-)))

(defun scheme-datum--special-subsequent? (ch)
  (or (scheme-datum--explicit-sign? ch)
      (eql ch ?.)
      (eql ch ?@)))

(defun scheme-datum--subsequent? (ch)
  (or (scheme-datum--initial? ch)
      (scheme-datum--digit? ch)
      (scheme-datum--special-subsequent? ch)))

(defun scheme-datum--dot-subsequent? (ch)
  (or (scheme-datum--sign-subsequent? ch)
      (eql ch ?.)))

(defun scheme-datum--sign-subsequent? (ch)
  (or (scheme-datum--initial? ch)
      (scheme-datum--explicit-sign? ch)
      (eql ch ?@)))

(defun scheme-datum-write-simple (obj &optional stream)
  "write OBJ in the R7RS Scheme datum value format.

Optional argument STREAM is the output stream (defaults to `standard-output')."
  (let ((stream (or stream standard-output)))
    (cond
     ((equal obj scheme-datum-true)
      (princ "#t" stream))
     ((equal obj scheme-datum-false)
      (princ "#f" stream))
     ((equal obj scheme-datum-nil-symbol)
      (princ "nil" stream))
     ((integerp obj)
      (princ obj stream))
     ((floatp obj)
      (princ (cond
              ((isnan obj) "+nan.0")
              ((= obj +1.0e+INF) "+inf.0")
              ((= obj -1.0e+INF) "-inf.0")
              (t obj))
             stream))
     ((listp obj)
      (scheme-datum--write-list obj stream))
     ((vectorp obj)
      (scheme-datum--write-vector obj stream))
     ((stringp obj)
      (scheme-datum--write-string obj stream ?\" ?\\))
     ((symbolp obj)
      (if (scheme-datum--simple-symbol? obj)
          (princ obj stream)
        (scheme-datum--write-string (symbol-name obj) stream ?| ?\\)))
     (t
      (error "Unsupported datum: %S" obj))))
  nil)

(provide 'scheme-datum)
;;; scheme-datum.el ends here
