;;; major-mode-hydra-test.el --- Tests for major-mode-hydra  -*- lexical-binding: t; -*-

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

(ert-deftest major-mode-hydra-test--define ()
  (let ((major-mode-hydra-title-generator (lambda (s) (format "%s bindings" s)))
        (major-mode-hydra-invisible-quit-key "q")
        (major-mode-hydra-separator "-"))
    (should (equal '(pretty-hydra-define major-mode-hydras/foo-mode
                     (:hint nil
                      :color teal
                      :title "foo-mode bindings"
                      :separator "-"
                      :quit-key "q" )
                     ("Foo"
                      (("a" foo "foo")
                       ("b" bar "bar" :a 1))))
                   (macroexpand-1 '(major-mode-hydra-define foo-mode nil
                                    ("Foo"
                                     (("a" foo "foo")
                                      ("b" bar "bar" :a 1)))))))))

(ert-deftest major-mode-hydra-test--define-multiple ()
  (let ((major-mode-hydra-title-generator (lambda (s) (format "%s bindings" s)))
        (major-mode-hydra-invisible-quit-key "q")
        (major-mode-hydra-separator "-"))
    (should (equal
             '(progn
               (pretty-hydra-define major-mode-hydras/foo-mode
                   (:hint nil :color teal :title "foo-mode bindings" :separator "-" :quit-key "q" )
                 ("Foo"
                  (("a" foo "foo")
                   ("b" bar "bar" :a 1))))
               (pretty-hydra-define major-mode-hydras/bar-mode
                   (:hint nil :color teal :title "bar-mode bindings" :separator "-" :quit-key "q" )
                 ("Foo"
                  (("a" foo "foo")
                   ("b" bar "bar" :a 1)))))

             (macroexpand-1 '(major-mode-hydra-define (foo-mode bar-mode) nil
                              ("Foo"
                               (("a" foo "foo")
                                ("b" bar "bar" :a 1)))))))))

(ert-deftest major-mode-hydra-test--define+ ()
  (let ((major-mode-hydra-title-generator (lambda (s) (format "%s bindings" s)))
        (major-mode-hydra-invisible-quit-key "q")
        (major-mode-hydra-separator "-"))
    (should (equal '(pretty-hydra-define+ major-mode-hydras/foo-mode
                     (:hint nil
                      :color teal
                      :title "foo-mode bindings"
                      :separator "-"
                      :quit-key "q" )
                     ("Foo"
                      (("a" foo "foo")
                       ("b" bar "bar" :a 1))))
                   (macroexpand-1 '(major-mode-hydra-define+ foo-mode nil
                                    ("Foo"
                                     (("a" foo "foo")
                                      ("b" bar "bar" :a 1)))))))))

(ert-deftest major-mode-hydra-test--use-package-normalize ()
  (should (equal '((foo-mode
                    nil
                    ("Foo"
                     ("a" foo "call foo")))
                   (foo-mode
                    (:title "Foo Commands")
                    ("Foo"
                     ("a" foo "call foo")))
                   (my-foo-mode
                    (:title "Foo Commands" :color teal)
                    ("Foo"
                     ("a" foo "call foo"))))
                 (major-mode-hydra--use-package-normalize
                  'foo-mode
                  :mode-hydra
                  '(("Foo"
                     ("a" foo "call foo"))
                    ((:title "Foo Commands")
                     ("Foo"
                      ("a" foo "call foo")))
                    (my-foo-mode
                     (:title "Foo Commands" :color teal)
                     ("Foo"
                      ("a" foo "call foo"))))))))

(provide 'major-mode-hydra-test)

;;; major-mode-hydra-test.el ends here
