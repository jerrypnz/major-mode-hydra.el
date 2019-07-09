;;; pretty-hydra-test.el --- Tests for pretty-hydra  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jerry Peng

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

(require 'use-package)
(require 'pretty-hydra)

(ert-deftest pretty-hydra-test--calc-column-width ()
  (dolist (test '((9 "C1" (("a" nil "c1")))
                  (13 "C1" (("a" nil "c1" :toggle t)))
                  (27 "C1" (("a" nil (foobar) :width 20)))
                  (27 "C1" (("a" nil (foobar) :width 20 :toggle t)))
                  (27 "C1" (("a" nil foobar :width 20)))
                  (13 "Long Column" (("a" nil "c1")))
                  (4  "C1" (("a" nil)))
                  (27 "C1" (("a" nil (foo))))))
    (-let [(expected col-name heads) test]
      (should (equal expected (pretty-hydra--calc-column-width col-name heads))))))

(ert-deftest pretty-hydra-test--normalize-head ()
  (dolist (test '((("a" nil "c1") . ("a" nil "c1"))
                  (("a" nil nil)   . ("a" nil))
                  (("a" foo "foo") . ("a" foo))
                  (("a" nil "c1" :exit t) . ("a" nil "c1" :exit t))
                  (("a" nil nil :exit t) . ("a" nil :exit t))
                  (("a" foo "foo" :exit t) . ("a" foo :exit t))))
    (-let [(expected . head) test]
      (should (equal expected (pretty-hydra--normalize-head! head))))))

(ert-deftest pretty-hydra-test--normalize-heads-plist ()
  (should (equal '("Foo"
                   (("a" nil nil)
                    ("b" foo "foo"))
                   "Bar"
                   (("c" nil "c1" :exit t)
                    ("d" nil nil :exit t)
                    ("e" foo "foo" :exit t)
                    ("f" (bar) nil :exit t)))
                 (pretty-hydra--normalize-heads-plist!
                  '("Foo"
                    (("a" nil)
                     ("b" foo))
                   "Bar"
                   (("c" nil "c1" :exit t)
                    ("d" nil :exit t)
                    ("e" foo :exit t)
                    ("f" (bar) :exit t)))))
          ))

(ert-deftest pretty-hydra-test--cell-docstring ()
  (dolist (test '(((" _a_: c1            ") . ("a" nil "c1"))
                  ((" _a_: %s(pretty-hydra-toggle \"c1\" (bound-and-true-p c1))        ") . ("a" c1 "c1" :toggle t))
                  ((" _a_: %s(pretty-hydra-toggle \"c1\" (bound-and-true-p status-var))        ") . ("a" c1 "c1" :toggle status-var))
                  ((" _a_: %s(pretty-hydra-toggle \"c1\" (status-expr))        ") . ("a" c1 "c1" :toggle (status-expr)))
                  ((" _a_: %s(pretty-hydra--pad-or-trunc-hint c1 14)") . ("a" nil c1))
                  ((" _a_: %s(pretty-hydra--pad-or-trunc-hint (c1) 14)") . ("a" nil (c1)))
                  ((" _a_: %s(pretty-hydra--pad-or-trunc-hint (c1 (quote foo)) 14)") . ("a" nil (c1 'foo)))
                  (nil   . ("a" nil))))
    (-let [(expected . head) test]
      (should (equal expected (pretty-hydra--cell-docstring 20 head))))))

(ert-deftest pretty-hydra-test--get-heads ()
  (should (equal '(("a" foo)
                   ("b" bar :exit t)
                   ("c" foobar :exit nil)
                   ("d" barfoo))
                 (pretty-hydra--get-heads
                  '("C1"
                    (("a" foo "foo")
                     ("b" bar nil :exit t :width 20))
                    "C2"
                    (("c" foobar (foo) :width 20 :exit nil)
                     ("d" barfoo)))))))

(ert-deftest pretty-hydra-test--merge-heads ()
  (should (equal '("C1"
                   (("a" foo "foo")
                    ("b" bar))
                   "C2"
                   (("c" foobar)
                    ("d" barfoo)))
                 (pretty-hydra--merge-heads
                  '("C1"
                    (("a" foo "foo")
                     ("b" bar)))
                  '("C2"
                    (("c" foobar)
                     ("d" barfoo))))))
  (should (equal '("C1"
                   (("a" foo "foo")
                    ("b" bar))
                   "C2"
                   (("c" foobar+)
                    ("d" barfoo+)))
                 (pretty-hydra--merge-heads
                  '("C1"
                    (("a" foo "foo")
                     ("b" bar))
                    "C2"
                    (("c" foobar)
                     ("d" barfoo)))
                  '("C2"
                    (("c" foobar+)
                     ("d" barfoo+))))))
  (should (equal '("C2"
                    (("c" foobar+)
                     ("d" barfoo+)))
                 (pretty-hydra--merge-heads
                  nil
                  '("C2"
                    (("c" foobar+)
                     ("d" barfoo+)))))))

(ert-deftest pretty-hydra-test--maybe-add-title ()
  (dolist (test '(("\n foo\ndocstring" . "foo")
                  ("\n %s`foo\ndocstring" . foo)
                  ("\n %s(foo \"bar\")\ndocstring" . (foo "bar"))
                  ("docstring" . nil)))
    (-let [(expected . title) test]
      (should (equal expected (pretty-hydra--maybe-add-title title "docstring"))))))

(ert-deftest pretty-hydra-test--get-cmds ()
  (should (equal '((bar . command) (foo . command))
                 (pretty-hydra--get-cmds '("Foo"
                   (("a" nil nil)
                    ("b" foo "foo"))
                   "Bar"
                   (("d" nil)
                    ("e" bar "foo" :exit t)
                    ("f" (foo bar) nil :exit t)))))))

(ert-deftest pretty-hydra-test--use-package-normalize ()
  (should (equal '((foo-hydra
                    nil
                    ("Foo"
                     ("a" foo "call foo")))
                   (foo-hydra
                    nil
                    ("Foo"
                     ("a" foo "call foo")))
                   (foo-hydra
                    nil
                    ("Foo"
                     ("a" foo "call foo")))
                   (foo-hydra
                    (:title "Foo Commands")
                    ("Foo"
                     ("a" foo "call foo")))
                   (you-foo-hydra
                    nil
                    ("Foo"
                     ("a" foo "call foo")))
                   (my-foo-hydra
                    (:title "Foo Commands" :color teal)
                    ("Foo"
                     ("a" foo "call foo")))
                   ((foo1 foo2)
                    (:title "Foo Commands" :color teal)
                    ("Foo"
                     ("a" foo "call foo")))
                   ((foo1 foo2)
                    nil
                    ("Foo"
                     ("a" foo "call foo"))))
                 (pretty-hydra--use-package-normalize
                  'foo
                  :pretty-hydra
                  '(("Foo"
                     ("a" foo "call foo"))
                    (("Foo"
                      ("a" foo "call foo")))
                    (nil
                     ("Foo"
                      ("a" foo "call foo")))
                    ((:title "Foo Commands")
                     ("Foo"
                      ("a" foo "call foo")))
                    (you-foo-hydra
                     ("Foo"
                      ("a" foo "call foo")))
                    (my-foo-hydra
                    (:title "Foo Commands" :color teal)
                    ("Foo"
                     ("a" foo "call foo")))
                    ((foo1 foo2)
                     (:title "Foo Commands" :color teal)
                     ("Foo"
                      ("a" foo "call foo")))
                    ((foo1 foo2)
                     ("Foo"
                      ("a" foo "call foo"))))))))

(ert-deftest pretty-hydra-test--use-package-add-keyword ()
  (let ((use-package-keywords '(:bind-keymap* :bind)))
    (pretty-hydra--use-package-add-keyword :pretty-hydra)
    (should (equal '(:bind-keymap* :pretty-hydra :bind) use-package-keywords))))

(provide 'pretty-hydra-test)

;;; pretty-hydra-test.el ends here
