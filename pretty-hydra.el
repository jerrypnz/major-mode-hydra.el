;;; pretty-hydra.el --- A macro for creating nice-looking hydras -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Version: 0.2.2
;; Package-Requires: ((hydra "0.15.0") (s "1.12.0") (dash "2.18.0") (emacs "24") (compat "29.1.4.1"))

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

(require 'compat)
(require 'dash)
(require 's)
(require 'hydra)

(defcustom pretty-hydra-enable-use-package t
  "Enable `use-package' integration when set to t."
  :type 'boolean
  :group 'pretty-hydra)

(defcustom pretty-hydra-default-title-body-format-spec "\n %s\n%s"
  "Default specification to format the title and body into a hydra docstring.
It should contain two `%s'. It can be used to customize the
whitespace around and between the title and the body. It is only
used if a title is provided. Can be overridden in the
pretty-hydra body via `:title-body-format-spec'."
  :type 'string
  :group 'pretty-hydra)

(defun pretty-hydra--normalize-head! (head)
  "Normalize HEAD so that it always have a hint."
  (-let [(_ cmd hint) head]
    (when (or (keywordp hint)
              (<= (length head) 2))
      (-let [hint (if (and cmd (symbolp cmd))
                      (symbol-name cmd)
                    nil)]
        (setcdr (cdr head) (cons hint (cddr head)))))
    head))

(defun pretty-hydra--normalize-heads-plist! (heads-plist)
  "Normalize HEADS-PLIST.  See `pretty-hydra--normalize-head'."
  (--each-indexed heads-plist
    (when (= (% it-index 2) 1)
      (dolist (head it)
        (pretty-hydra--normalize-head! head))))
  heads-plist)

(defun pretty-hydra--cell-width (key hint-width)
  "Calculate the width of a head cell based on the KEY and HINT-WIDTH."
  (+ 6 (length key) hint-width))

(defconst pretty-hydra--default-hint-width 20)

(defun pretty-hydra--calc-column-width (column-name heads)
  "Calculate the width for a column based on COLUMN-NAME and HEADS."
  (->> heads
       (-map (-lambda ((key _ hint &plist :width width :toggle toggle-p))
               (cond
                ((char-or-string-p hint)
                 (pretty-hydra--cell-width key (+ (length hint) (if toggle-p 4 0)))) ; string hint
                ((numberp width)
                 (pretty-hydra--cell-width key width)) ; configured width
                ((or (null hint))
                 0)
                (t
                 (pretty-hydra--cell-width key pretty-hydra--default-hint-width))))) ; dynamic hint
       (cons (+ 2 (length column-name)))
       -max))

(defun pretty-hydra--pad-or-trunc-hint (hint len)
  "Pad or truncate HINT to LEN, preserving text properties."
  (if (null hint)
      hint
    (let ((len1 (length hint))
          (props (text-properties-at 0 hint)))
      (cond
       ((> len1 len) (let ((x (s-truncate len hint)))
                       (when props
                         (set-text-properties 0 len props x))
                       x))
       ((< len1 len) (s-pad-right len " " hint))
       (t            hint)))))

(defun pretty-hydra--cell-docstring (width head)
  "Generate docstring for a HEAD with given WIDTH."
  (-let* (((key cmd hint &plist :toggle toggle-p) head)
          (escaped-key (s-replace "^" "\\^" key))
          ;; If any character gets escpaed, we need to increase the
          ;; width as well because the espace char \ is not displayed.
          (width (+ width (- (length escaped-key) (length key)))))
    (cond
     ((char-or-string-p hint)
      (if toggle-p
          (let* ((status-expr (cond
                               ((eq toggle-p t)  `(bound-and-true-p ,cmd))
                               ((listp toggle-p) toggle-p)
                               (t                `(bound-and-true-p ,toggle-p))))
                 (expr (prin1-to-string `(pretty-hydra-toggle ,hint ,status-expr)))
                 (width (+ width (- (+ (length expr) 2) (+ (length hint) 4)))))
            (list (s-pad-right width " " (format " _%s_: %%s%s" escaped-key expr))))
        (-let* ((escaped-hint (s-replace "^" "\\^" hint))
                (width (+ width (- (length escaped-hint) (length hint)))))
          (list (s-pad-right width " " (format " _%s_: %s" escaped-key escaped-hint)))))) ;; string hint

     ((or (null hint))
      nil)  ;; no hint, doesn't show it in docstring at all

     (t
      (list (format " _%s_: %%s%s"
                    escaped-key
                    (prin1-to-string
                     `(pretty-hydra--pad-or-trunc-hint ,hint ,(- width (length key) 5)))))))))

(defun pretty-hydra--gen-heads-docstring (column-name separator heads max-heads)
  "Generate pretty docstring for one column.
COLUMN-NAME appears in the first row, followed by the SEPARATOR
in the second row.  After that are all the hydra HEADS, each of
which consists of the key and hint.  If the number of HEADS is
smaller than MAX-HEADS, extra lines are created at the end which
is necessary to create the final table."
  (let* ((width (pretty-hydra--calc-column-width column-name heads))
         (rows (-mapcat (-partial #'pretty-hydra--cell-docstring width) heads)))
    (-concat (list (s-pad-right width " " (format " %s^^" column-name))
                   (s-pad-right width separator ""))
             rows
             ;; Add empty rows if it doesn't have as many heads in this column
             (-repeat (- max-heads (length rows)) (s-pad-left width " " "^^")))))

(defun pretty-hydra--gen-body-docstring (separator hydra-plist)
  "Generate hydra body docstring based on the HYDRA-PLIST.
SEPARATOR char is used to generate the separator line."
  (-let* ((head-columns (-partition 2 hydra-plist))
          (max-heads (->> head-columns
                          (-map (-lambda ((_ heads))
                                  (-count (-lambda ((_ _ hint)) hint)
                                          heads)))
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

(defun pretty-hydra--get-heads (heads-plist)
  "Extract key, command and options from the HEADS-PLIST.
This is used to create the HEADS to be passed to `defhydra'."
  (->> heads-plist
       (-partition 2)
       (-mapcat #'cadr)
       (-map (-lambda ((key cmd _ . opts))
               (-concat (list key cmd) (pretty-hydra--remove-custom-opts opts))))))

(defun pretty-hydra--maybe-add-title (title title-body-format-spec docstring)
  "Add TITLE to DOCSTRING according to TITLE-BODY-FORMAT-SPEC.

If TITLE-BODY-FORMAT-SPEC is nil, the value of
`pretty-hydra-default-title-body-format-spec' is used."
  (if (null title)
      docstring
    (let ((format-spec (if title-body-format-spec
                           title-body-format-spec
                         pretty-hydra-default-title-body-format-spec)))
      (format format-spec
              (cond
               ((char-or-string-p title) title)
               ((symbolp title)          (format "%%s`%s" title))
               ((listp title)            (format "%%s%s" (prin1-to-string title)))
               (t                        ""))
              docstring))))

(defconst pretty-hydra--opts '(:separator :formatter :title :quit-key :width :toggle))

(defun pretty-hydra--remove-custom-opts (body)
  "Remove custom options used by pretty hydra from the hydra BODY."
  (->> body
       (-partition 2)
       (-remove (-lambda ((opt _)) (member opt pretty-hydra--opts)))
       (-mapcat #'identity)))

(defun pretty-hydra--dedupe-heads (heads)
  "Return HEADS with duplicates removed.
Two heads are considered duplicate if they have the same key."
  (->> heads
       (-group-by #'car)
       (-map (-lambda ((_ . xs)) (-last-item xs)))))

(defun pretty-hydra--merge-heads (old new)
  "Merge items from NEW plist into the OLD plist.
The result is a new plist."
  (let ((cols (cl-loop for (key _value) on new by 'cddr collect key)))
    (-reduce-from (lambda (acc x)
                    (compat-call plist-put acc x
                                 (pretty-hydra--dedupe-heads
                                  (append (compat-call plist-get acc x #'equal)
                                          (compat-call plist-get new x #'equal)))
                                 #'equal))
                  old
                  cols)))

(defun pretty-hydra--generate (name body heads-plist)
  "Helper function to generate expressions with given NAME, BODY, HEADS-PLIST.
See `pretty-hydra-define' and `pretty-hydra-define+'."
  (let* ((separator (or (compat-call plist-get body :separator) "─"))
         (title     (compat-call plist-get body :title))
         (formatter (or (compat-call plist-get body :formatter)
                        #'identity))
         (title-body-format-spec (plist-get body :title-body-format-spec))
         (quit-key  (compat-call plist-get body :quit-key))
         (docstring (->> heads-plist
                         (pretty-hydra--gen-body-docstring separator)
                         (pretty-hydra--maybe-add-title title title-body-format-spec)
                         (funcall formatter)
                         (s-prepend "\n"))) ;; This is required, otherwise the docstring won't show up correctly
         (heads (pretty-hydra--get-heads heads-plist))
         (heads (if quit-key
                    (if (listp quit-key)
                        (append heads (--map (list it nil) quit-key))
                      (append heads `((,quit-key nil))))
                  heads))
         (body (compat-call plist-put body :hint nil #'equal)))
    `(progn
       (eval-and-compile
         (set (defvar ,(intern (format "%S/heads-plist" name))
                nil
                ,(format "heads-plist of %S." name))
              (quote ,heads-plist))
         (set (defvar ,(intern (format "%S/pretty-body" name))
                nil
                ,(format "pretty-body of %S." name))
              (quote ,body)))
       (defhydra ,name ,(pretty-hydra--remove-custom-opts body)
         ,docstring
         ,@heads))))

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
    docstring as a title of the hydra.

  - `:formatter' a function that takes the generated docstring
    and return a decorated one.  It can be used to further
    customize the hydra docstring.

  - `:quit-key' a key of list of keys for quitting the hydra.
    When specified, invisible head(s) are created with the
    specified keys for quitting the hydra.

HEADS-PLIST is a plist of columns of hydra heads.  The keys of
the plist should be column names.  The values should be lists of
hydra heads.  Each head has exactly the same syntax as that of
`defhydra', except hint is required for the head to appear in the
docstring.  The following additional options are supported:

  - `:width' the max width of a dynamic hint, used to calculate
    the final width of the entire column.  It is ignored when the
    hint is a string.

  - `:toggle' when specified, it makes the head a toggle and adds
    an indicator to the end of the hint for the status of the
    toggle.  The value of this option can be a symbol, an s-exp
    or t.  The toggle status is read from the given variable, by
    evaluating the given expression or checking the `cmd' as if
    it's a variable.  The latter is especially useful for minior
    modes, e.g.

       (\"n\" `linum-mode' \"line number\" :toggle t)"
  (declare (indent defun))
  (pretty-hydra--generate name body (pretty-hydra--normalize-heads-plist! heads-plist)))

(defun pretty-hydra--prop-or-nil (name prop-name)
  "Return value of PROP-NAME for hydra with given NAME, or nil if the property doesn't exist."
  (let ((s (intern (concat (symbol-name name) prop-name))))
    (when (boundp s)
      (symbol-value s))))

;;;###autoload
(defmacro pretty-hydra-define+ (name body heads-plist)
  "Redefine an existing pretty-hydra by adding new HEADS-PLIST.
If heads are added to a column already in NAME, the heads are
appended to that column.  Existing BODY is replaced with the new
one if specified.  Arguments are the same as `pretty-hydra-define'."
  (declare (indent defun))
  (pretty-hydra--generate
   name
   (or body (pretty-hydra--prop-or-nil name "/pretty-body"))
   (pretty-hydra--merge-heads
    (pretty-hydra--prop-or-nil name "/heads-plist")
    (pretty-hydra--normalize-heads-plist! heads-plist))))

(defface pretty-hydra-toggle-on-face
  '((t (:inherit 'font-lock-keyword-face)))
  "Face used to render titles for pretty hydra."
  :group 'pretty-hydra)

(defface pretty-hydra-toggle-off-face
  '((t (:inherit 'font-lock-comment-face)))
  "Face used to render titles for pretty hydra."
  :group 'pretty-hydra)

;;;###autoload
(defun pretty-hydra-toggle (name status)
  "Create a dynamic hint that look like a radio button with given NAME.
Radio is considered on when STATUS is non-nil, otherwise off."
  (s-concat name " " (if status
                         (propertize "(*)" 'face 'pretty-hydra-toggle-on-face)
                       (propertize "( )" 'face 'pretty-hydra-toggle-off-face))))

(defun pretty-hydra--get-cmds (heads-plist)
  "Return a list of all the commands that appear in HEADS-PLIST.
s-expressions are not included."
  (let (cmds)
    (cl-loop
     for (_ heads) on heads-plist by #'cddr
     do (cl-loop
         for (_ cmd) in heads
         when (and cmd (symbolp cmd))
         do (push (cons cmd 'command) cmds)))
    cmds))

(defconst pretty-hydra--use-package-arg-error
  ":pretty-hydra wants a heads-plist and optionally a name and/or body in front")

(defun pretty-hydra--normalize-args (default-name args)
  "Normalize `use-package' `:pretty-hydra' keyword ARGS using DEFAULT-NAME.
ARGS are normalized to a list of NAME, BODY and HEADS-PLIST.
DEFAULT-NAME is used if no name is given."
  (cond
   ((not (listp args))
    (use-package-error pretty-hydra--use-package-arg-error))
   ;; only the heads-plist, use default-name
   ;; default body
   ((stringp (car args))
    `(,default-name nil ,args))
   ;; only the heads-plist but in a nested list
   ((and (= (length args) 1) (stringp (caar args)))
    `(,default-name nil ,@args))
   ;; body/name + heads-plist
   ((and (= (length args) 2) (stringp (caadr args)))
    (if (and (listp (car args))
             (or (keywordp (caar args))
                 (null (caar args))))
        `(,default-name ,@args) ;body + heads, use default-name
      (cons (car args) (cons nil (cdr args))))) ;name + heads, insert nil body
   ;; name + body + heads-plist, return as is
   ((and (= (length args) 3) (stringp (caaddr args)))
    args)
   (t
    (use-package-error pretty-hydra--use-package-arg-error))))

(declare-function use-package-concat "use-package-core")
(declare-function use-package-process-keywords "use-package-core")
(declare-function use-package-error "use-package-core")
(defvar use-package-keywords)

(defun pretty-hydra--use-package-normalize (package _keyword arglists)
  "Normalize `use-package' `:pretty-hydra' keyword ARGLISTS for PACKAGE."
  (let* ((default-name (intern (format "%s-hydra" package))))
    (-map (-partial #'pretty-hydra--normalize-args default-name) arglists)))

(defun pretty-hydra--use-package-handler (package _keyword args rest state)
  "Generate pretty-hydra defs for PACKAGE using ARGS with `use-package' STATE and REST keywords."
  (use-package-concat
   (use-package-process-keywords package rest state)
   (-map (-lambda ((name body heads-plist))
           `(pretty-hydra-define+ ,name ,body ,heads-plist))
         args)))

(defun pretty-hydra--use-package-autoloads (_pkg-name _keyword args)
  "Return a list of `use-package' autoloads for commands found in ARGS."
  (-mapcat (-lambda ((_ _ heads-plist)) (pretty-hydra--get-cmds heads-plist)) args))

(defun pretty-hydra--use-package-add-keyword (keyword)
  "Add the KEYWORD to `use-package-keywords'."
  (setq use-package-keywords
          ;; should go in the same location as :bind
          (cl-loop for item in use-package-keywords
                   if (eq item :bind-keymap*)
                   collect :bind-keymap* and collect keyword
                   else
                   ;; don't add duplicates
                   unless (eq item keyword)
                   collect item)))

(defun pretty-hydra--enable-use-package ()
  "Enable `use-package' integration.
Called automatically when `use-package' is present and
`pretty-hydra-enable-use-package' is set to t."
  (with-eval-after-load 'use-package-core
    (pretty-hydra--use-package-add-keyword :pretty-hydra)
    (defalias 'use-package-normalize/:pretty-hydra #'pretty-hydra--use-package-normalize)
    (defalias 'use-package-autoloads/:pretty-hydra #'pretty-hydra--use-package-autoloads)
    (defalias 'use-package-handler/:pretty-hydra #'pretty-hydra--use-package-handler)))

(when pretty-hydra-enable-use-package
  (pretty-hydra--enable-use-package))

(provide 'pretty-hydra)

;;; pretty-hydra.el ends here
