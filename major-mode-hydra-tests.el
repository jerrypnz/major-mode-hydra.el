;;; major-mode-hydra-tests.el --- Tests for major-mode-hydra  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>

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

(require 'major-mode-hydra)

;;TODO Remove me
(setq major-mode-hydra--heads-alist
      '((emacs-lisp-mode . ("Test Emacs"      (("v" emacs-version "Emacs Version"))))
        (restclient-mode . ("Test RestClient" (("v" emacs-version "Emacs Version"))))))

;; TODO Remove me
(major-mode-hydra--get-or-recompile 'restclient-mode)

;; TODO Add proper tests

(provide 'major-mode-hydra-tests)

;;; major-mode-hydra-tests.el ends here
