;;; pretty-hydra.el --- A macro for creating nice-looking hydras -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Version: 0.1.0
;; Package-Requires: ((hydra "0.13.4") (s "1.10.0") (dash "2.12.1") (emacs "24"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides a macro, `pretty-hydra-define', which defines a hydra with
;; column for each group of heads.

;;; Code:

(require 'dash)
(require 's)
(require 'hydra)

(defun pretty-hydra--calc-column-width (column-name heads)
  "Calculate the width for a column based on COLUMN-NAME and HEADS."
  (->> heads
       (-map (-lambda ((key _ hint))
               (cond
                ((char-or-string-p hint) (+ 7 (length key) (length hint))) ;; string hint
                ((or (null hint) (symbolp hint)) 0) ;; no hint
                (t 17))))   ;; dynamic hint (TODO trim to 10 chars long)
       (cons (+ 2 (length column-name)))
       -max))

(defun pretty-hydra--gen-heads-docstring (column-name separator heads max-heads)
  "Generate pretty docstring for one column.
COLUMN-NAME appears in the first row, followed by the SEPARATOR
in the second row.  After that are all the hydra HEADS, each of
which consists of the key and hint.  If the number of HEADS is
smaller than MAX-HEADS, extra lines are created at the end which
is necessary to create the final table."
  (-let ((column-len (pretty-hydra--calc-column-width column-name heads)))
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
           (-concat (list (format " %s^^" column-name)
                          (format "%s" (s-pad-right column-len separator "")))
                    docs
                    ;; Add empty rows if it doesn't have as many heads in this column
                    (-repeat (- max-heads (length docs)) (s-pad-left column-len " " "^^")))
           (-map (lambda (doc) (s-pad-right column-len " " doc)) docs))))

(defun pretty-hydra--gen-body-docstring (separator hydra-plist)
  "Generate hydra body docstring based on the HYDRA-PLIST.
SEPARATOR char is used to generate the separator line."
  (-let* ((head-columns (-partition 2 hydra-plist))
          (max-heads (->> head-columns
                          (-map (-lambda ((_ heads)) (length heads)))
                          -max))
          (head-docstrings (-map (-lambda ((column-name heads))
                                   (pretty-hydra--gen-heads-docstring column-name separator heads max-heads))
                                 head-columns)))
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
  "Extract key, command and options from the HYDRA-PLIST.
This is used to create the HEADS to be passed to `defhydra'."
  (->> hydra-plist
       (-partition 2)
       (-mapcat #'cadr)
       (-map (-lambda ((head &as key cmd hint . opts))
               (if (char-or-string-p hint)
                   (-concat (list key cmd) opts)
                 head)))))

(defface pretty-hydra-title-face
  '((t (:inherit 'default)))
  "Face used to render titles for pretty hydra"
  :group 'pretty-hydra)

(defun pretty-hydra--title-formatter (title)
  "Create a docstring formatter which add the `TITLE' to the docstring."
  `(lambda (docstring)
     (s-concat " " (propertize ,title 'face 'pretty-hydra-title-face) "\n" docstring)))

(defconst pretty-hydra--opts '(:separator :formatter :title :quit-key))

(defun pretty-hydra--remove-custom-opts (body)
  "Remove custom options used by pretty hydra from the hydra BODY."
  (->> body
       (-partition 2)
       (-remove (-lambda ((opt _)) (member opt pretty-hydra--opts)))
       (-mapcat #'identity)))

;;;###autoload
(defmacro pretty-hydra-define (name body heads-plist)
  "Define a pretty hydra with given NAME, BODY options and HEADS-PLIST.
The generated hydra has a nice-looking docstring which is a table
with columns of command keys and hints.

NAME should be a symbol and is passed to `defhydra' as is.

BODY is the same as that in `defhydra', withe the following
pretty hydra specific ones:

  - `:separator' a single char used to generate the separator
    line.

  - `:title' a string that's added to the beginning of the
    docstring as a title of the hydra.  Ignored when `:formatter'
    is also specified.

  - `:formatter' a function that takes the generated docstring
    and return a decorated one.  It can be used to further
    customize the hydra docstring.

  - `:quit-key' a key for quitting the hydra.  When specified, an
    invisible head is created with this key for quitting the
    hydra.

HEADS-PLIST is a plist of columns of hydra heads.  The keys of
the plist should be column names.  The values should be lists of
hydra heads.  Each head has exactly the same syntax as that of
`defhydra', except hint is required for the head to appear in the
docstring."
  (declare (indent defun))
  (let* ((separator (or (plist-get body :separator) "─"))
         (formatter (or (plist-get body :formatter)
                        (-some-> (plist-get body :title)
                                 pretty-hydra--title-formatter)
                        #'identity))
         (quit-key (plist-get body :quit-key))
         (docstring (->> heads-plist
                         (pretty-hydra--gen-body-docstring separator)
                         (funcall formatter)
                         (s-prepend "\n"))) ;; This is required, otherwise the docstring won't show up correctly
         (heads (pretty-hydra--get-heads heads-plist))
         (heads (if quit-key
                    (append heads `((,quit-key nil)))
                  heads))
         (body (pretty-hydra--remove-custom-opts body)))
    `(defhydra ,name ,body
       ,docstring
       ,@heads)))

(provide 'pretty-hydra)

;;; pretty-hydra.el ends here
