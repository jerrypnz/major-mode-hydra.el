;;; major-mode-hydra.el --- Major mode keybindings managed by Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Version: 0.2.2
;; Package-Requires: ((dash "2.18.0") (pretty-hydra "0.2.2") (emacs "25"))

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

;; Inspired by Spacemacs major mode leader key and based on the
;; awesome hydra, this package offers a better way to manage your
;; major mode specific key bindings.

;;; Code:

(require 'compat)
(require 'dash)
(require 'pretty-hydra)

(defcustom major-mode-hydra-separator "═"
  "The separator char to be used to draw the separator line.
UTF-8 box drawing characters are recommended."
  :type 'string
  :group 'major-mode-hydra)

(defcustom major-mode-hydra-title-generator nil
  "Title generator, a function used to generate a title for major mode hydras.
The function should take a single parameter, which is the major
mode name (a symbol), and return a string."
  :type 'function
  :group 'major-mode-hydra)

(defcustom major-mode-hydra-invisible-quit-key nil
  "Key for the invisible hydra head that quits the hydra.
Set to nil to stop generating such heads."
  :type 'key-sequence
  :group 'major-mode-hydra)

(defun major-mode-hydra--put-if-absent (plist prop val)
  "Set PROP to VAL if it's absent in PLIST."
  (when (not (compat-call plist-get plist prop #'equal))
    (compat-call plist-put plist prop val #'equal))
  plist)

(defun major-mode-hydra--name-for (mode)
  "Return a symbol which is the major MODE hydra name."
  (intern (format "major-mode-hydras/%s" mode)))

(defun major-mode-hydra--body-name-for (mode)
  "Return a symbol which is the body function name for the major MODE hydra."
  (intern (format "major-mode-hydras/%s/body" mode)))

(defun major-mode-hydra--generate (mode body heads-plist &optional overwrite-p)
  "Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Overwrite existing hydra if OVERWRITE-P is t, otherwise add new heads to it."
  (let* ((hydra-name (major-mode-hydra--name-for mode))
         (title (when (functionp major-mode-hydra-title-generator)
                  (funcall major-mode-hydra-title-generator mode)))
         (body (-> (compat-call plist-put body :hint nil #'equal)
                   (major-mode-hydra--put-if-absent :color 'teal)
                   (major-mode-hydra--put-if-absent :title title)
                   (major-mode-hydra--put-if-absent :separator major-mode-hydra-separator)
                   (major-mode-hydra--put-if-absent :quit-key major-mode-hydra-invisible-quit-key)))
         (df    (if overwrite-p 'pretty-hydra-define 'pretty-hydra-define+)))
    `(,df ,hydra-name ,body ,heads-plist)))

;;;###autoload
(defmacro major-mode-hydra-define (mode body heads-plist)
  "Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Overwrite existing hydra if there is one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST."
  (declare (indent defun))
  (if (listp mode)
      `(progn
         ,@(-map (lambda (m) (major-mode-hydra--generate m body (copy-tree heads-plist) t))
                 mode))
    (major-mode-hydra--generate mode body heads-plist t)))

;;;###autoload
(defmacro major-mode-hydra-define+ (mode body heads-plist)
  "Generate a major mode hydra for given MODE with given BODY and HEADS-PLIST.
Add new heads if there is already an existing one.

MODE can also be a list of modes in which case the same hydras
are created for all these modes.  Useful in multiple closely
related major modes.

Refer to `pretty-hydra-define' for documentation about BODY and HEADS-PLIST."
  (declare (indent defun))
  (if (listp mode)
      `(progn
         ,@(-map (lambda (m) (major-mode-hydra--generate m body (copy-tree heads-plist)))
                 mode))
    (major-mode-hydra--generate mode body heads-plist)))

;;;###autoload
(defmacro major-mode-hydra-bind (mode column &rest bindings)
  "Add BINDINGS (heads) for a MODE under the COLUMN.

MODE is the major mode name (symbol).  There is no need to quote it.

COLUMN is a string to put the hydra heads under.

BINDINGS is a list of hydra heads to be added.  Each head has
exactly the same structure as that in `pretty-hydra-define' or
`defhydra', except `:exit' is set to t by default."
  (declare (indent 2)
           (obsolete major-mode-hydra-define+ "July 2019"))
  `(major-mode-hydra-define+ ,mode nil ,(list column bindings)))

(defun major-mode-hydra-dispatch (mode)
  "Summon the hydra for given MODE (if there is one)."
  (let ((orig-mode mode))
    (catch 'done
      (while mode
        (let ((hydra (major-mode-hydra--body-name-for mode)))
          (when (fboundp hydra)
            (call-interactively hydra)
            (throw 'done t)))
        (setq mode (get mode 'derived-mode-parent)))
      (user-error "Major mode hydra not found for %s or its parent modes" orig-mode))))

;;;###autoload
(defun major-mode-hydra ()
  "Show the hydra for the current major mode."
  (interactive)
  (major-mode-hydra-dispatch major-mode))

(declare-function use-package-concat "use-package-core")
(declare-function use-package-process-keywords "use-package-core")
(defvar use-package-keywords)

(defun major-mode-hydra--use-package-normalize (package _keyword arglists)
  "Normalize `use-package' `:major-mode-hydra' keyword ARGLISTS for PACKAGE."
  (-map (-partial #'pretty-hydra--normalize-args package) arglists))

(defun major-mode-hydra--use-package-handler (package _keyword args rest state)
  "Generate major-mode-hydra defs for PACKAGE using ARGS with `use-package' STATE and REST keywords."
  (use-package-concat
   (use-package-process-keywords package rest state)
   (-map (-lambda ((name body heads-plist))
           `(major-mode-hydra-define+ ,name ,body ,heads-plist))
         args)))

(defun major-mode-hydra--use-package-autoloads (_pkg-name _keyword args)
  "Return a list of `use-package' autoloads for commands found in ARGS."
  (-mapcat (-lambda ((_ _ heads-plist)) (pretty-hydra--get-cmds heads-plist)) args))

(defun major-mode-hydra--enable-use-package ()
  "Enable `use-package' integration.
Called automatically when `use-package' is present and
`pretty-hydra-enable-use-package' is set to t."
  (with-eval-after-load 'use-package-core
    (pretty-hydra--use-package-add-keyword :mode-hydra)
    (defalias 'use-package-normalize/:mode-hydra #'major-mode-hydra--use-package-normalize)
    (defalias 'use-package-autoloads/:mode-hydra #'major-mode-hydra--use-package-autoloads)
    (defalias 'use-package-handler/:mode-hydra #'major-mode-hydra--use-package-handler)))

(when pretty-hydra-enable-use-package
  (major-mode-hydra--enable-use-package))

(provide 'major-mode-hydra)

;;; major-mode-hydra.el ends here
