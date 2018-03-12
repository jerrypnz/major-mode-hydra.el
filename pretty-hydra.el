;;; pretty-hydra.el --- A macro for creating nice-looking hydras -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'hydra)

(defun pretty-hydra--calc-column-width (group-name heads)
  (->> heads
       (-map (-lambda ((key _ hint))
               (cond
                ((char-or-string-p hint) (+ 7 (length key) (length hint))) ;; string hint
                ((or (null hint) (symbolp hint)) 0) ;; no hint
                (t 17))))   ;; dynamic hint (TODO trim to 10 chars long)
       (cons (+ 2 (length group-name)))
       -max))

(defun pretty-hydra--gen-heads-docstring (group-name heads max-heads)
  (-let ((column-len (pretty-hydra--calc-column-width group-name heads)))
    (-as-> heads docs
           (-mapcat (-lambda ((key _ hint))
                      (cond
                       ((char-or-string-p hint) ;; string hint
                        (list (format " [_%s_] %s" key hint)))
                       ((or (null hint) (symbolp hint)) ;; no hint, doesn't show it in docstring at all
                        nil)
                       (t  ;; dynamic hint (TODO trim to 10 chars long)
                        (list (format " [_%s_] ?%s?" key key)))))
                    docs)
           (-concat (list (format " %s^^" group-name)
                          (format "%s" (s-pad-right column-len "─" "")))
                    docs
                    ;; Add empty rows if it doesn't have as many heads in this column
                    (-repeat (- max-heads (length docs)) (s-pad-left column-len " " "^^")))
           (-map (lambda (doc) (s-pad-right column-len " " doc)) docs))))

(defun pretty-hydra--gen-body-docstring (hydra-plist)
  (-let* ((head-groups (-partition 2 hydra-plist))
          (max-heads (->> head-groups
                          (-map (-lambda ((_ heads)) (length heads)))
                          -max))
          (head-docstrings (-map (-lambda ((group-name heads))
                                   (pretty-hydra--gen-heads-docstring group-name heads max-heads))
                                 head-groups)))
    (->> head-docstrings
         (apply #'-zip)
         (-map-indexed (lambda (i ss)
                         (s-join (if (= i 1) "" " ")
                                 (if (listp (cdr ss))
                                     ss
                                   (cons (car ss) (cons (cdr ss) nil)))))) ;; cons-pair to list because of -zip
         (s-join "\n")
         (format "\n%s\n"))))

(defun pretty-hydra--get-heads (hydra-plist)
  (->> hydra-plist
       (-partition 2)
       (-mapcat #'cadr)
       (-map (-lambda ((head &as key cmd hint . opts))
               (if (char-or-string-p hint)
                   (-concat (list key cmd) opts)
                 head)))))

(defmacro pretty-hydra-define (name body heads-plist)
  (declare (indent defun) (doc-string 3))
  (let ((docstring (pretty-hydra--gen-body-docstring heads-plist))
        (heads (pretty-hydra--get-heads heads-plist)))
    `(defhydra ,name ,body
       ,docstring
       ,@heads)))

(provide 'pretty-hydra)

;;; pretty-hydra.el ends here
