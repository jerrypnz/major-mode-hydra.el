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

(ert-deftest major-mode-hydra-test--bind-key--empty-heads-alist ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("v" emacs-version "Emacs Version"))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "Emacs Version")))))))

(ert-deftest major-mode-hydra-test--bind-key--cmd-name-as-hint ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("v" emacs-version))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version")))))))

(ert-deftest major-mode-hydra-test--bind-key--head-opts ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("v" emacs-version :exit nil :color red))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version" :exit nil :color red)))))))

(ert-deftest major-mode-hydra-test--bind-key--hint-and-head-opts ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("v" emacs-version "Emacs Version" :exit nil :color red))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "Emacs Version" :exit nil :color red)))))))

(ert-deftest major-mode-hydra-test--bind-key--add-head-to-existing-column ()
  (let ((major-mode-hydra--heads-alist '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version")))))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("b" foobar "Foobar"))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "b" foobar "Foobar")
                                      ("Test Emacs" "v" emacs-version "emacs-version")))))))

(ert-deftest major-mode-hydra-test--bind-key--new-column ()
  (let ((major-mode-hydra--heads-alist '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version")))))
    (major-mode-hydra-bind emacs-lisp-mode "Foo"
      ("b" foobar "Foobar"))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Foo"        "b" foobar "Foobar")
                                      ("Test Emacs" "v" emacs-version "emacs-version")))))))

(ert-deftest major-mode-hydra-test--bind-key--duplicate-key ()
  (let ((major-mode-hydra--heads-alist '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version")))))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("v" foobar "Foobar"))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode  ("Test Emacs" "v" emacs-version "emacs-version")))))))

(ert-deftest major-mode-hydra-test--bind-key--nil-cmd ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("q" nil "quit"))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "q" nil "quit")))))))

(ert-deftest major-mode-hydra-test--bind-key--nil-cmd-no-hint ()
  (let ((major-mode-hydra--heads-alist nil))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("q" nil))
    (should (equal major-mode-hydra--heads-alist
                   '((emacs-lisp-mode ("Test Emacs" "q" nil "nil")))))))

(ert-deftest major-mode-hydra-test--bind-key--invisibe-quit ()
  (let* ((major-mode-hydra--heads-alist '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version"))))
         (major-mode-hydra-invisible-quit-key "q"))
    (major-mode-hydra-bind emacs-lisp-mode "Test Emacs"
      ("q" foobar "foobar"))
    (should (equal major-mode-hydra--heads-alist '((emacs-lisp-mode ("Test Emacs" "v" emacs-version "emacs-version")))))))

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
