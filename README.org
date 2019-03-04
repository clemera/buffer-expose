#+BEGIN_HTML
<a href="https://elpa.gnu.org/packages/buffer-expose.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/favicon.png"/></a>
#+END_HTML

* Description

Visual buffer switching using a window grid ([[https://github.com/abo-abo/ace-window][ace-window ]]key hints are optional):

[[./images/grid-aw.png]]

* Installation

For manual installation, clone the repository and call:

#+BEGIN_SRC elisp
(package-install-file "/path/to/buffer-expose.el")
#+END_SRC

* Config

To use the default bindings for switching buffers with buffer-expose
use buffer-expose-mode:

#+BEGIN_SRC elisp
(buffer-expose-mode 1)
#+END_SRC

The default bindings are defined in buffer-expose-mode-map:

#+BEGIN_SRC elisp
(defvar buffer-expose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<s-tab>") 'buffer-expose)
    (define-key map (kbd "<C-tab>") 'buffer-expose-no-stars)
    (define-key map (kbd "C-c <C-tab>") 'buffer-expose-current-mode)
    (define-key map (kbd "C-c C-m") 'buffer-expose-major-mode)
    (define-key map (kbd "C-c C-d") 'buffer-expose-dired-buffers)
    (define-key map (kbd "C-c C-*") 'buffer-expose-stars)
    map)
  "Mode map for command `buffer-expose-mode'.")
#+END_SRC

There are user options to customize which buffers are shown and you can easily
write your own command, like this:

#+BEGIN_SRC elisp
(defun my-expose-command (&optional max)
  (interactive "P")
  (buffer-expose-show-buffers
    <your-buffer-list> max [<hide-regexes> <filter-func>]))
#+END_SRC
