;;; major-mode-hydra.el --- Major mode keybindings managed by Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Keywords: hydra
;; Version: 0.1.0
;; Package-Requires: ((dash "2.12.1"))

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

;;; Code:

(require 'dash)
(require 'pretty-hydra)

(defvar major-mode-hydra--heads-alist nil
  "An alist holding hydra heads for each major mode, keyed by the mode name.")

(defvar major-mode-hydra--body-cache nil
  "An alist holding compiled hydras for each major mode. Whenever
  `major-mode-hydra--heads-alist' is changed, the hydra for
  the mode gets recompiled.")

(defun major-mode-hydra--recompile (mode heads-plist)
  (let ((hydra-name (make-symbol (format "major-mode-hydras/%s" mode)))
        ;; By default, exit hydra after invoking a head and warn if a foreign key is pressed.
        (hydra-body '(:exit t :hint nil :foreign-keys warn)))
    (eval `(pretty-hydra-define ,hydra-name ,hydra-body ,heads-plist))))

(defun major-mode-hydra--get-or-recompile (mode)
  (-if-let (hydra (alist-get mode major-mode-hydra--body-cache))
      hydra
    (-when-let (heads-plist (alist-get mode major-mode-hydra--heads-alist))
      (let ((hydra (major-mode-hydra--recompile mode heads-plist)))
        (setf (alist-get mode major-mode-hydra--body-cache) hydra)
        hydra))))

(defun major-mode-hydra--update-heads (heads-plist column key command hint)
  (-if-let (existing (major-mode-hydra--get-existing-binding heads-plist key))
      (progn
        (message "\"%s\" has already been bound to %s" (car existing) (cadr existing))
        heads-plist)
    (-let [column-heads (lax-plist-get heads-plist column)]
      (lax-plist-put heads-plist
                     column
                     (nconc column-heads `((,key ,command ,hint)))))))

(defun major-mode-hydra--get-existing-binding (heads-plist key)
  (-as-> heads-plist x
         (-slice x 1 (length x) 2)
         (-mapcat #'identity x)
         (assoc key x)))

(defun major-mode-hydra-bind-key (mode column key command &optional hint)
  (-as-> (alist-get mode major-mode-hydra--heads-alist) heads-plist
         (major-mode-hydra--update-heads heads-plist
                                         column key command
                                         (or hint (symbol-name command)))
         (setf (alist-get mode major-mode-hydra--heads-alist)
               heads-plist)))

(defun major-mode-hydra ()
  (interactive)
  (let* ((mode major-mode)
         (hydra (major-mode-hydra--get-or-recompile mode)))
    (if hydra
        (call-interactively hydra)
      (message "Major mode hydra not found for %s" mode))))

(provide 'major-mode-hydra)

;;; major-mode-hydra.el ends here
