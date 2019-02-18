;;; major-mode-hydra.el --- Major mode keybindings managed by Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Version: 0.1.0
;; Package-Requires: ((dash "2.15.0") (pretty-hydra "0.1.1") (emacs "25"))

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

(defvar major-mode-hydra--heads-alist nil
  "An alist holding hydra heads for each major mode, keyed by the mode name.")

(defvar major-mode-hydra--body-cache nil
  "An alist holding compiled hydras for each major mode.

Whenever `major-mode-hydra--heads-alist' is changed, the hydra
for the mode gets recompiled.")

(defun major-mode-hydra--recompile (mode heads)
  "Recompile the hydra for given MODE with given HEADS definition."
  (let* ((hydra-name (make-symbol (format "major-mode-hydras/%s" mode)))
         (title (when (functionp major-mode-hydra-title-generator)
                  (funcall major-mode-hydra-title-generator mode)))
         ;; By default, exit hydra after invoking a head and warn if a foreign key is pressed.
         (hydra-body `(:color teal
                              :hint nil
                              :title ,title
                              :separator ,major-mode-hydra-separator
                              :quit-key ,major-mode-hydra-invisible-quit-key))
         ;; Convert heads to a plist that `pretty-hydra-define' expects.
         (hydra-heads-plist (->> heads
                                 reverse
                                 (-group-by #'car)
                                 (-mapcat (-lambda ((column . heads)) (list column (-map #'cdr heads)))))))
    (eval `(pretty-hydra-define ,hydra-name ,hydra-body ,hydra-heads-plist))))

(defun major-mode-hydra--get-or-recompile (mode)
  "Get cached hydra for given MODE, or recompile it if there isn't a cached one."
  (-if-let (hydra (alist-get mode major-mode-hydra--body-cache))
      hydra
    (-when-let (heads (alist-get mode major-mode-hydra--heads-alist))
      (let ((hydra (major-mode-hydra--recompile mode heads)))
        (setf (alist-get mode major-mode-hydra--body-cache) hydra)
        hydra))))

(defun major-mode-hydra--update-heads (heads column bindings)
  "Update hydra HEADS in the given COLUMN with BINDINGS.
BINDINGS is a list of hydra heads that are added to the COLUMN.
Return updated HEADS."
  (-reduce-from (-lambda (heads (key command . hint-and-plist))
                  (-let [(hint . plist) (if (and (car hint-and-plist)
                                                 (not (keywordp (car hint-and-plist))))
                                            hint-and-plist
                                          (cons (symbol-name command) hint-and-plist))]
                    (-if-let ((_ _ cmd) (-first (lambda (h) (equal (cadr h) key)) heads))
                        (progn
                          (message "\"%s\" has already been bound to %s" key cmd)
                          heads)
                      (if (and major-mode-hydra-invisible-quit-key
                               (equal key major-mode-hydra-invisible-quit-key))
                          (progn
                            (message "\"%s\" has already been bound to the invisible quit" major-mode-hydra-invisible-quit-key)
                            heads)
                        (cons `(,column ,key ,command ,hint ,@plist) heads)))))
                heads
                bindings))

(defun major-mode-hydra--bind-key (mode column bindings)
  "Add BINDINGS (heads) for a MODE under the COLUMN."
  (-as-> (alist-get mode major-mode-hydra--heads-alist) heads
         (major-mode-hydra--update-heads heads column bindings)
         (setf (alist-get mode major-mode-hydra--heads-alist)
               heads))
  ;; Invalidate cached hydra for the mode
  (setq major-mode-hydra--body-cache
        (assq-delete-all mode major-mode-hydra--body-cache)))

(defun major-mode-hydra--unbind-all (mode)
  "Remove all the hydra heads for MODE.  Introduced for testing."
  (setq major-mode-hydra--body-cache
        (assq-delete-all mode major-mode-hydra--body-cache))
  (setq major-mode-hydra--heads-alist
        (assq-delete-all mode major-mode-hydra--heads-alist)))

(defun major-mode-hydra-clear-cache ()
  "Clear major mode hydra cache.
Cached hydras are then recreated the next time `major-mode-hydra'
gets executed.  Useful when debugging an issue or force update a
major mode hydra."
  (interactive)
  (setq major-mode-hydra--body-cache nil))

;; Use a macro so that it's not necessary to quote things

;;;###autoload
(defmacro major-mode-hydra-bind (mode column &rest bindings)
  "Add BINDINGS (heads) for a MODE under the COLUMN.

MODE is the major mode name (symbol).  There is no need to quote it.

COLUMN is a string to put the hydra heads under.

BINDINGS is a list of hydra heads to be added.  Each head has
exactly the same structure as that in `pretty-hydra-define' or
`defhydra', except `:exit' is set to t by default."
  (declare (indent 2))
  `(major-mode-hydra--bind-key ',mode ,column ',bindings))

(defun major-mode-hydra-dispatch (mode)
  "Summon the hydra for given MODE (if there is one)."
  (let ((hydra (major-mode-hydra--get-or-recompile mode)))
    (if hydra
        (call-interactively hydra)
      (message "Major mode hydra not found for %s" mode))))

;;;###autoload
(defun major-mode-hydra ()
  "Show the hydra for the current major mode."
  (interactive)
  (major-mode-hydra-dispatch major-mode))

(provide 'major-mode-hydra)

;;; major-mode-hydra.el ends here
