
## Major Mode Hydra

[![MELPA](https://melpa.org/packages/major-mode-hydra-badge.svg)](https://melpa.org/#/major-mode-hydra)
[![MELPA Stable](https://stable.melpa.org/packages/major-mode-hydra-badge.svg)](https://stable.melpa.org/#/major-mode-hydra)

Inspired by [Spacemacs major mode leader
key](http://spacemacs.org/doc/DOCUMENTATION.html#major-mode-leader-key)
and based on the awesome [hydra](https://github.com/abo-abo/hydra),
this package offers a better way to manage your major mode specific
key bindings.

## Install

### package.el

This package is available on [MELPA](https://melpa.org).

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `major-mode-hydra` <kbd>[RET]</kbd>

### Manual

This package depends on [hydra](https://github.com/abo-abo/hydra),
[dash](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el). Make sure they are installed
first.

Download the source code and put it wherever you like and add the
directory to the load path:

```elisp
(add-to-list 'load-path "/place/where/you/put/it/")
```

## Usage

`require` the package and bind the `major-mode-hydra` command to a key:

```elisp
(require 'major-mode-hydra)
(global-set-key (kbd "C-M-m") 'major-mode-hydra)
```

or if you prefer `use-package`:

```elisp
(use-package major-mode-hydra
  :bind
  ("C-M-m" . major-mode-hydra))
```

Whenever the command `major-mode-hydra` is executed, a (hopefully)
pretty hydra for the major mode of the current buffer pops up.

![example1](screenshots/example1.png)

Use the `major-mode-hydra-bind` macro to add heads to a major mode
hydra. The following is an example for `clojure-mode` (as shown in the
above screenshot):

```elisp
(major-mode-hydra-bind clojure-mode "Connect"
  ("j" cider-jack-in "jack-in")
  ("J" cider-jack-in-clojurescript "jack-in-cljs")
  ("c" cider-connect "connect")
  ("R" cider-restart "restart")
  ("Q" cider-quit "quit"))
(major-mode-hydra-bind clojure-mode "Load"
  ("k" cider-load-buffer "buffer")
  ("l" cider-load-file "file")
  ("L" cider-load-all-project-ns "all-ns")
  ("r" cider-refresh "reload"))
```

It takes the name of a major mode (it should be an unquoted symbol), a
column name and a list of hydra heads under that column. Each head is
defined exactly in the same way as `defhydra`. You can call
`major-mode-hydra-bind` multiple times for the same major mode in
different places. Every time it's called, the hydra for that major
mode will be recreated the next time `major-mode-hydra` command is
used.

The generated hydra has the following default options:

```elisp
(:color teal :hint nil)
```

which means by default it quits the hydra after a head command is
executed. For major mode commands, this should usually be what you
want. You can override it for each head using head options:

```elisp
(major-mode-hydra-bind clojure-mode "Load"
  ("k" cider-load-buffer "buffer" :exit nil)
  ("l" cider-load-file "file" :color red))
```


### Customization

#### Custom separator

You can customize the separator by setting the custom varable
`major-mode-hydra-separator`. It should be set to a string containing
a single character which is used to draw the separator line. [Unicode
box drawing
characters](https://en.wikipedia.org/wiki/Box-drawing_character) are
recommended.

#### Add an invisible hydra head for quitting

You can set `major-mode-hydra-invisible-quit-key` to a key sequence
which can be used for quitting the hydra. This key doesn't show up in
the docstring. The key being used is not allowed in
`major-mode-hydra-bind`, otherwise there can be conflicts.

#### Add title to the hydra

You can add a title to the major mode hydra by setting
`major-mode-hydra-title-generator`, which is a function that takes the
major mode symbol and returns a string. For example, the title in the
above screenshot is generated with the following generator:

``` elisp
(setq major-mode-hydra-title-generator
      '(lambda (mode)
         (s-concat "\n"
                   (s-repeat 10 " ")
                   (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                   " "
                   (symbol-name mode)
                   " commands")))
```

## Pretty Hydra

[![MELPA](https://melpa.org/packages/pretty-hydra-badge.svg)](https://melpa.org/#/pretty-hydra)
[![MELPA Stable](https://stable.melpa.org/packages/pretty-hydra-badge.svg)](https://stable.melpa.org/#/pretty-hydra)

This package includes `pretty-hydra.el` which is used by
`major-mode-hydra.el` but can also be installed and used on its own.

The following is an example in my own configuration:

```elisp
(pretty-hydra-define jp-window (:hint nil :foreign-keys warn :quit-key "q")
  (;; general window management commands
   "Windows" (("x" ace-delete-window "delete")
              ("m" ace-delete-other-windows "maximize")
              ("s" ace-swap-window "swap")
              ("a" ace-select-window "select")
              ("o" other-window "cycle"))
   ;; resize
   "Resize" (("h" move-border-left "←")
             ("j" move-border-down "↓")
             ("k" move-border-up "↑")
             ("l" move-border-right "→")
             ("n" balance-windows "balance"))
   ;; split
   "Split"  (("b" split-window-right "horizontally")
             ("B" split-window-horizontally-instead "horizontally instead")
             ("v" split-window-below "vertically")
             ("V" split-window-vertically-instead "vertically instead"))
   ;; zoom
   "Zoom" (("+" zoom-in "in")
           ("=" zoom-in)
           ("-" zoom-out "out")
           ("0" jp-zoom-default "reset"))))
```

Apart from hydra's options like `:hint` or `:color`, there are
additional options that allow you to customize the generated body
docstring:

- `:title` adds a title to the docstring. If it is a elisp variable or
  sexp, it's evaluated every time the hydra is opened or refreshed.
- `:formatter` allows you to fully customize the docstring. It's a
  function that takes the docstring `pretty-hydra-define` generates,
  and returns a new docstring that's gonna be used. You can do things
  like generating a border etc.
- `:quit-key` adds a invisible hydra head for quitting the hydra. It
  can be very useful when you set `:foreign-keys` to `warn`.

Hint for each head can be dynamic, either a symbol or an sexp which
gets evaluated dynamically when rendering the hydra. Dynamic hint is
always padded with space, or trimmed so that its length is fixed. This
is to ensure the pretty layout doesn't get broken. You can specify the
expected `:width` in head plists.

Dynamic hints are useful in toggles where it shows different hint
based on the state of the toggle. pretty-hydra has built-in support
for such use cases. You can enable it by setting `:toggle` to `t` in
head plist. In this case, the command should also be a variable that
indicates whether the toggle is on or off. This is the case for pretty
much all minor modes. `:toggle` can also be set to a symbol or s-exp
in which case it's evaluated to get the status of the toggle.

The following is an example toggles hydra:

``` elisp
(pretty-hydra-define jp-toggles
  (:hint nil :color amaranth :quit-key "q" :title "Toggles")
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t))
   "Highlight"
   (("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p))
   "Debug"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))
```

![example2](screenshots/example2.png)

The on/off faces can be customized through
`pretty-hydra-toggle-on-face` and `pretty-hydra-toggle-off-face`.

You can use `pretty-hydra-define+` in order to add heads to an already
existing `pretty-hydra`. New heads are appended to existing columns,
if their names match (otherwise new columns are created). Here's an
example redefining the `jp-window` hydra we created above.

```elisp
(pretty-hydra-define+ jp-window ()
  (;; these heads are added to the existing "Windows" column
   "Windows" (("r" transpose-frame "rotate")
              ("z" zone "zone out!"))
   ;; this is a new column, which gets added
   "Appearance" (("f" set-frame-font "font")
                 ("t" load-theme "theme"))))
```

## License

Copyright © 2018 Jerry Peng

Distributed under the GNU General Public License, version 3
