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

(require 'pretty-hydra)

(ert-deftest pretty-hydra-test--calc-column-width ()
  (dolist (test '((10 "C1" (("a" nil "c1")))
                  (28 "C1" (("a" nil (foobar) :width 20)))
                  (28 "C1" (("a" nil foobar :width 20)))
                  (13 "Long Column" (("a" nil "c1")))
                  (4 "C1" (("a" nil)))
                  (28 "C1" (("a" nil (foo))))))
    (-let [(expected col-name heads) test]
      (should (equal expected (pretty-hydra--calc-column-width col-name heads))))))

(ert-deftest pretty-hydra-test--normalize-head ()
  (dolist (test '((("a" nil "c1") . ("a" nil "c1"))
                  (("a" nil)   . ("a" nil))
                  (("a" nil "c1" :exit t) . ("a" nil "c1" :exit t))
                  (("a" nil nil :exit t) . ("a" nil :exit t))))
    (-let [(expected . head) test]
      (should (equal expected (pretty-hydra--normalize-head head))))))

(ert-deftest pretty-hydra-test--cell-docstring ()
  (dolist (test '(((" [_a_] c1           ") . ("a" nil "c1"))
                  ((" [_a_] %s(pretty-hydra--pad-or-trunc-hint c1 13)") . ("a" nil c1))
                  ((" [_a_] %s(pretty-hydra--pad-or-trunc-hint (c1) 13)") . ("a" nil (c1)))
                  ((" [_a_] %s(pretty-hydra--pad-or-trunc-hint (c1 (quote foo)) 13)") . ("a" nil (c1 'foo)))
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

(provide 'pretty-hydra-test)

;;; pretty-hydra-test.el ends here
