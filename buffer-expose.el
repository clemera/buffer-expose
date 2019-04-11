;;; buffer-expose.el --- Visual buffer switching using a window grid  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Clemens Radermacher <clemera@posteo.net>
;; URL: https://github.com/clemera/buffer-expose
;; Version: 0.4.1
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shows buffers in a window grid with miniaturized previews from which you
;; can select a buffer to switch to. Inspired by the exposé feature of some
;; window managers.

;;; Code:

(require 'cl-lib)

;; silence byte compiler
(declare-function face-remap-remove-relative "ext:face-remap")
;; optional deps
(defvar exwm-input-line-mode-passthrough nil)
(defvar aw-dispatch-function 'aw-dispatch-default)
(defvar avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?t . avy-action-teleport)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?y . avy-action-yank)
    (?i . avy-action-ispell)
    (?z . avy-action-zap-to-char)))
(defvar aw-ignored-buffers '("*Calc Trail*" "*LV*"))
(defvar aw-background t)
(defvar aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
(declare-function aw-switch-to-window, "ext:ace-window")
(declare-function aw--lead-overlay "ext:ace-window")
(declare-function aw-select "ext:ace-window")
(declare-function aw-update "ext:ace-window")
(declare-function avy-handler-default "ext:avy")

;; * Minor mode

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

;;;###autoload
(define-minor-mode buffer-expose-mode
  "Expose buffers.

Instantiate bindings of `buffer-expose-mode-map'."
  :global t
  :keymap buffer-expose-mode-map)

;; * Customization

(defgroup buffer-expose nil
  "Visual buffer switching using a window grid"
  :group 'convenience
  :prefix "buffer-expose-")

(defface buffer-expose-selected-face '((t :inherit highlight))
  "Background face for selected window.")

(defvar buffer-expose-rescale-factor 0.3
  "The rescale factor for buffer contents showed in the overview.")

(defvar buffer-expose-mode-line-title-func #'buffer-name
  "Function to call to display the title in the mode-line.

Should return the string to display.")

(defface buffer-expose-mode-line-face
  '((t nil))
  "Face for titles shown in modelines.")

(defface buffer-expose-ace-char-face
  '((t :inherit font-lock-warning-face))
  "Face for avy chars in modelines.")

(defcustom buffer-expose-aw-keys '(?a ?d ?g ?h ?j ?l ?e ?i ?w ?o ?c ?m)
  "Keys for selecting windows with avy."
  :type '(repeat character))

(defcustom buffer-expose-auto-init-aw nil
  "Whether to start with ace-window activated."
  :type 'boolean)

(defun buffer-expose-choose-default-action (buf)
  "Restore inital window config and switch to choosen buffer BUF."
  (buffer-expose-reset)
  (switch-to-buffer buf))

(defcustom buffer-expose-choose-action-func #'buffer-expose-choose-default-action
  "Action to execute after choosing a candidate from the grid.

The function recieves one argument which is the buffer of the
choosen window."
  :type 'function)

(defcustom buffer-expose-one-buffer-function #'switch-to-buffer
  "Function to use when there is only one buffer to display.

Takes one argument which is the buffer in question."
  :type 'function)

(defcustom buffer-expose-switch-to-buffer-func #'switch-to-buffer
  "Command to use for `buffer-expose-switch-to-buffer' command."
  :type 'function)

(defcustom buffer-expose-max-num-windows 12
  "Maximal number of windows per page to display.

Numerical prefixes given to interactive commands allow to
override this value."
  :type 'integer)

(defcustom buffer-expose-max-num-buffers 0
  "Maximal number of buffers to collect for expose view.

A value if 0 means no limit."
  :type 'integer)

(defcustom buffer-expose-highlight-selected t
  "Whether to highlight the selected window of the overview."
  :type 'boolean)

(defcustom buffer-expose-show-current-buffer nil
  "Whether to show the current buffer (on invocation) in the overview."
  :type 'boolean)

(defcustom buffer-expose-grid-alist
  '((64 . (8 . 8))
    (32 . (8 . 4))
    (24 . (8 . 3))
    (18 . (6 . 3))
    (16 . (4 . 4))
    (12 . (4 . 3))
    (11 . (4 . 3))
    (10 . (4 . 3))
    (9  . (4 . 3))
    (8  . (4 . 2))
    (7  . (4 . 2))
    (6  . (3 . 2))
    (5  . (3 . 2))
    (4  . (2 . 2))
    (3  . (2 . 2))
    (2  . (2 . 1))
    (1  . (1 . 1)))
  "Rules for the amount of windows and how to display them.

The `car' contains the number of buffers to display and is mapped
to a display rule. Each display rule is a cell (columns . rows)
which defines the number of colums and the number of rows per
page. If you always want the same grid layout set
`buffer-expose-default-rule'. See also `buffer-expose--get-rule'
for the algorithm for choosing the layout rule."
  :type '(alist :key-type integer
                :value-type (cons interger interger)))

(defcustom buffer-expose-default-rule nil
  "Default rule for grid layout.

The rule format is a cell of (columns . rows) which defines the
number of colums and the number of rows per page. If set the grid
will always use this layout regardless how many buffers are
available for display."
  :type '(cons integer integer))

(defcustom buffer-expose-hide-modelines nil
  "Whether to hide modelines in the overview."
  :type 'boolean)

(defcustom buffer-expose-hide-headerlines nil
  "Whether to hide headerlines of buffers in the overview."
  :type 'boolean)

(defcustom buffer-expose-hide-regexes nil
  "List of regexes for buffer names which should be hidden."
  :type '(repeat string))

(defcustom buffer-expose-hide-cursor t
  "Whether to hide cursors in the overview."
  :type 'boolean)

(defcustom buffer-expose-hide-cursor-in-other-windows t
  "Whether to hide cursors in other windows in the overview."
  :type 'boolean)

(defcustom buffer-expose-wrap-vertically t
  "Whether to wrap around on vertical movement."
  :type 'boolean)

(defcustom buffer-expose-key-hint
  (concat "Navigate with TAB, Shift-TAB, n, p, f, b, [, ]. "
          "Press RET, SPC or click to choose a buffer, k to kill, q to abort. "
          "See buffer-expose-grid-map for more.")
  "Help message when overview is shown."
  :type 'string)

(defvar buffer-expose-grid-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'buffer-expose-left-window)
    (define-key map (kbd "b") 'buffer-expose-left-window)
    (define-key map (kbd "<right>") 'buffer-expose-right-window)
    (define-key map (kbd "f") 'buffer-expose-right-window)
    (define-key map (kbd "p") 'buffer-expose-up-window)
    (define-key map (kbd "<up>") 'buffer-expose-up-window)
    (define-key map (kbd "<down>") 'buffer-expose-down-window)
    (define-key map (kbd "n") 'buffer-expose-down-window)
    (define-key map (kbd "a") 'buffer-expose-first-window-in-row)
    (define-key map (kbd "e") 'buffer-expose-last-window-in-row)
    (define-key map (kbd "s") 'buffer-expose-switch-to-buffer)
    (define-key map (kbd "<") 'buffer-expose-first-window)
    (define-key map (kbd ">") 'buffer-expose-last-window)
    (define-key map (kbd "SPC") 'buffer-expose-ace-window)
    (define-key map (kbd ",") 'buffer-expose-ace-window)
    (define-key map (kbd "TAB") 'buffer-expose-next-window)
    (define-key map (kbd "<tab>") 'buffer-expose-next-window)
    (define-key map (kbd "<S-iso-lefttab>") 'buffer-expose-prev-window)
    (define-key map (kbd "]") 'buffer-expose-next-page)
    (define-key map (kbd "[") 'buffer-expose-prev-page)
    (define-key map "k" 'buffer-expose-kill-buffer)
    map)
  "Transient keymap used for the overview.")

(defvar buffer-expose-exit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'buffer-expose-handle-mouse)
    (define-key map [mouse-2] 'buffer-expose-handle-mouse)
    (define-key map [mouse-3] 'buffer-expose-handle-mouse)
    (define-key map (kbd "RET") 'buffer-expose-choose)
    (define-key map (kbd "<return>") 'buffer-expose-choose)
    (define-key map (kbd "C-g") 'buffer-expose-reset)
    (define-key map (kbd "q") 'buffer-expose-reset)
    (define-key map [t] 'ignore)
    map)
  "Map to handle exit commands of the overview.")

;; * internal state tracking

(defvar buffer-expose--empty-buffer-name " *buffer-expose-empty*")
(defvar buffer-expose--cancel-overriding-map-function nil)

(defvar buffer-expose--next-stack nil)
(defvar buffer-expose--prev-stack nil)
(defvar buffer-expose--buffer-list nil
  "Holds buffers left for display in grid.")

(defvar buffer-expose--last-buffer nil
  "Last buffer that was selected.")

(defvar buffer-expose--ace-p nil)

(defun buffer-expose--update-display ()
  "If buffer has changed, update highlighting."
  (let ((new-buf (window-buffer)))
    (unless (eq new-buf buffer-expose--last-buffer)
      ;; remove hl
      (when (buffer-live-p buffer-expose--last-buffer)
        (with-current-buffer buffer-expose--last-buffer
          (buffer-expose--set-current-buffer-background t)))
      (unless (minibufferp new-buf)
        ;; hl the new one
        (with-current-buffer new-buf
          (buffer-expose--set-current-buffer-background))
        ;; update remembered buffer
        (setq buffer-expose--last-buffer new-buf)))))

(defvar buffer-expose--selected-cookie nil
  "Current remapping cookie for selected buffer.")

(defun buffer-expose--set-current-buffer-background (&optional reset)
  "Set buffer background using `buffer-expose-selected-face'.

If RESET is non-nil reset the buffer background."
  (when (cond ((not buffer-expose-highlight-selected)
               t)
              ((and (not reset) (not buffer-expose--selected-cookie))
                 (setq buffer-expose--selected-cookie
                       (face-remap-add-relative 'default
                                                'buffer-expose-selected-face)))
              ((and reset buffer-expose--selected-cookie)
               (face-remap-remove-relative buffer-expose--selected-cookie)
               (setq buffer-expose--selected-cookie nil)
               t))
    (force-window-update (current-buffer))))


(defvar buffer-expose--initial-window-config nil
  "Storing the initial window configuration.")

(cl-defstruct (buffer-expose--bdata (:constructor buffer-expose--bdata-create)
                                    (:copier nil))
  buffer
  cursor
  read-only
  mode-line
  header-line
  cookie
  boundaries)

(defvar buffer-expose--buffer-data nil
  "List of `buffer-expose--bdata' structures.

Each entry holds per buffer information for reset.")

(defvar buffer-expose--reactivate-modes nil
  "Minor Modes which need to be reenabled.")

(defvar buffer-expose--redisable-modes nil
  "Minor Modes which need to be redisabled.")

(defvar buffer-expose--reset-variables nil
  "Global variables which need to be reset.")

(defun buffer-expose--filter-buffer-list (bl max &optional
                                      show-current
                                      hide-regexes
                                      filter-fun)
  "Filter buffers for display.

BL is the list of buffers to filter.

MAX is the maximal number of buffers to use.

SHOW-CURRENT:
if non nil consider current buffer as well.

HIDE-REGEXES:
don't add buffers matching any of regexes

FILTER-FUN:
buffers for which this function returns nil are ignored."
  (let ((res ())
        (buffer nil)
        (n 0))
    (while (and (or (not max)
                    (< n max))
                (setq buffer (pop bl)))
      (when (and (or show-current
                     (not (eq buffer (current-buffer))))
                 (or (not hide-regexes)
                     (not (cl-find-if (lambda (regex)
                                        (string-match regex (buffer-name buffer)))
                                      hide-regexes)))
                 (not (minibufferp buffer))
                 (not (string-match "\\`\\*helm" (buffer-name buffer)))
                 (not (string-match "\\`\\*Completions" (buffer-name buffer)))
                 (not (string-match "\\` " (buffer-name buffer)))
                 (or (not filter-fun)
                     (with-current-buffer buffer
                       (funcall filter-fun buffer))))
        (setq n (1+ n))
        (push buffer res)))
    (nreverse res)))

(defun buffer-expose--get-rule (num max)
  "Get expose display rule.

The rule is choosen based on NUM number of buffers and MAX amount
of windows per page (see `buffer-expose-grid-alist'). If MAX is
nil it defaults to `buffer-expose-max-num-windows'. If there are
less buffers available than windows the first rule which
corresponds to the number of buffers in
`buffer-expose-grid-alist' is choosen. This can be overidden by
`buffer-expose-default-rule'."
  (or buffer-expose-default-rule
      (let ((nums (mapcar 'car buffer-expose-grid-alist)))
        (while (and nums
                    ;; fewer buffers than rule
                    (or (< num (car nums))
                        ;; qrule exceeds limit
                        (> (car nums) (or max
                                          buffer-expose-max-num-windows))))
          (pop nums))
        (when nums
          (cdr (assq (car nums)
                     buffer-expose-grid-alist))))))

(defun buffer-expose--get-major-modes ()
  "Get a list of available major modes."
  (let (modes mode)
    (dolist (buf (buffer-list) modes)
      (setq mode (buffer-local-value 'major-mode buf))
      (unless (memq mode modes)
        (push mode modes)))))

(defun buffer-expose--get-mode-buffers (mode)
  "Get all buffers with ‘major-mode’ MODE."
  (let (bufs)
    (dolist (buf (buffer-list) (nreverse bufs))
      (when (eq mode (buffer-local-value 'major-mode buf))
        (push buf bufs)))))

;; * Grid

(defun buffer-expose--other-window ()
  "Select `next-window' without affecting buffer list."
  (let ((w (next-window (selected-window) 'never)))
    (select-window w :no-record)))

(defvar buffer-expose--window-list nil)

(defun buffer-expose-create-grid (x y)
  "Create window grid with X columns, Y rows.

Return list of windows created."
  (let ((window-min-width 0)
        (window-min-height 0)
        (window-combination-resize t)
        (ws (list (selected-window))))
    (delete-other-windows)
    (dotimes (_ (1- y))
      (split-window-vertically)
      (dotimes (_ (1- x))
        (push (split-window-horizontally) ws)
        (buffer-expose--other-window))
      (buffer-expose--other-window)
      (push (selected-window) ws))
    (dotimes (_ (1- x))
      (push (split-window-horizontally) ws)
      (buffer-expose--other-window))
    (balance-windows)
    (nreverse ws)))

(defvar-local buffer-expose--empty-buffer nil)

(defun buffer-expose--create-empty-buffer (&optional name)
  "Create buffer for empty window with name NAME.

NAME defaults to `buffer-expose--empty-buffer-name'."
  (with-current-buffer (generate-new-buffer
                        (or name buffer-expose--empty-buffer-name))
    (setq-local buffer-expose--empty-buffer t)
    (setq buffer-read-only t)
    (setq mode-line-format "")
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows nil)
    (current-buffer)))

(defun buffer-expose-fill-grid ()
  "Fill grid windows."
  (let ((ws buffer-expose--window-list))
    (dolist (w ws)
      (if buffer-expose--buffer-list
          (with-current-buffer (pop buffer-expose--buffer-list)
            ;; buffer data
            (push (buffer-expose--bdata-create
                   :buffer (current-buffer)
                   :cursor cursor-type
                   :read-only buffer-read-only
                   :mode-line mode-line-format
                   :header-line header-line-format
                   :cookie (face-remap-add-relative 'default
                                                    :height buffer-expose-rescale-factor)
                   :boundaries indicate-buffer-boundaries)
                  buffer-expose--buffer-data)

            ;; prevent changing contents
            (setq buffer-read-only t)
            (setq indicate-buffer-boundaries nil)
            (setq mode-line-format (if buffer-expose-hide-modelines
                                       nil
                                     '(""
                                       (buffer-expose--ace-p
                                        (:propertize
                                         (:eval (window-parameter (selected-window) 'ace-window-path))
                                         face buffer-expose-ace-char-face)
                                        " ")
                                       " "
                                       (:propertize (:eval (funcall buffer-expose-mode-line-title-func))
                                                    face buffer-expose-mode-line-face))))


            (if buffer-expose-hide-cursor
                (setq cursor-type nil))
            (if buffer-expose-hide-headerlines
                (setq header-line-format nil))

            (setf (window-buffer w) (current-buffer)))
        (setf (window-buffer w)
              (buffer-expose--create-empty-buffer))))))

(defun buffer-expose--empty-window-p (w)
  "Check if window W is an empty one."
  (with-current-buffer (window-buffer w)
    buffer-expose--empty-buffer))

(defun buffer-expose--select-window (w)
  "Select window W.

Prevents switching to empty windows. Does not change the order of
`buffer-list'. After selection the grid view is updated."
  ;; dont put buffer at front when selecting windows
  (select-window w :no-record)
  ;; redisplay
  (buffer-expose--update-display))


(defun buffer-expose-show-buffers (blist &optional max regexes filter)
  "Init buffer expose and display grid of buffers.

This function is intended to be used when creating new buffer expose
commands.

BLIST is the list of buffers to display.

MAX is the maximum of windows to display per page and is passed
to `prefix-numeric-value' if non nil.

REGEXES is a list of regexes for buffer names to hide and is merged with
`buffer-expose-hide-regexes'.

FILTER is a filter function which has to return non-nil for buffers
which should be included."
  (buffer-expose--show-buffers
   ;; default filters
   (buffer-expose--filter-buffer-list
    blist
    (and (/= buffer-expose-max-num-buffers 0)
         buffer-expose-max-num-buffers)
    buffer-expose-show-current-buffer
    (append buffer-expose-hide-regexes regexes)
    filter)
   (and max (prefix-numeric-value max))))

(defun buffer-expose--init-map ()
  "Initilize the transient map for overview."
  (let ((map (make-composed-keymap buffer-expose-grid-map buffer-expose-exit-map)))
    (setq buffer-expose--cancel-overriding-map-function
          (set-transient-map
           map
           (lambda ()
             (not (lookup-key buffer-expose-exit-map (this-command-keys-vector))))))))

(defun buffer-expose--init-ui ()
  "Initilize user interface."
  ;; highlight first window
  (with-current-buffer (window-buffer (frame-first-window))
    (setq buffer-expose--last-buffer (current-buffer))
    (buffer-expose--set-current-buffer-background))

  (setq  exwm-input-line-mode-passthrough t)
  (buffer-expose--init-map)

  ;; some buffers (dired and maybe more) need this to display correctly


  ;; setup new window-switch behaviour
  (buffer-expose--select-window (frame-first-window))
  ;; initil message how to use
  (message buffer-expose-key-hint)
  (when buffer-expose-auto-init-aw
    (buffer-expose-ace-window))
  (dolist (w (window-list nil 'nomini))
    (with-current-buffer (window-buffer w)
      (redisplay))))

(defvar buffer-expose-fringe nil)

(defun buffer-expose--save-state ()
  "Save current state."
  (setq buffer-expose--initial-window-config (current-window-configuration))
  ;; variables
  (dolist (var '(cursor-in-non-selected-windows
                 mouse-autoselect-window
                 mouse-1-click-follows-link))
    (push (cons var (symbol-value var))
          buffer-expose--reset-variables))

  (when (boundp 'fringe-mode)
    (let ((p (frame-parameters)))
      (setq buffer-expose-fringe (list fringe-mode
                                       (assq 'left-fringe p)
                                       (assq 'right-fringe p)))))

  ;; minor modes
  (dolist (mode '(scroll-bar-mode window-divider-mode))
    (when (boundp mode)
      (if (symbol-value mode)
          (push mode buffer-expose--reactivate-modes)
        (push mode buffer-expose--redisable-modes))))

  (setq mouse-autoselect-window nil
        mouse-1-click-follows-link nil)

  (when buffer-expose-hide-cursor-in-other-windows
    (setq cursor-in-non-selected-windows nil))

  (when (fboundp 'fringe-mode)
    (fringe-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (let ((window-divider-default-places t))
    (window-divider-mode 1)))


(defun buffer-expose--show-buffers (blist max)
  "Initalize buffer expose and display first page.

BLIST is the list of buffers to display.

MAX is the maximum of windows to display per page."
  (let* ((blist
         ;; shared between commands...
          (setq buffer-expose--buffer-list
                blist))
         (rule (buffer-expose--get-rule (length blist) max)))
    (cond ((not buffer-expose--buffer-list)
           (error "No buffers to display"))
          ((not rule)
           (error "No display rule found"))
          ((not (cdr buffer-expose--buffer-list))
           (funcall buffer-expose-one-buffer-function
                    (car buffer-expose--buffer-list)))
          (t
           (let* ((cols (car rule))
                  (rows (cdr rule)))
             (buffer-expose--save-state)
             (setq buffer-expose--window-list
                   (buffer-expose-create-grid cols rows))
             (buffer-expose-fill-grid)
             (buffer-expose--init-ui))))))

;; * Reset state

(defun buffer-expose--reset-buffers ()
  "Reset buffers."
  ;; reset the seleted one
  ;; remove any previous ones...
  (dolist (data buffer-expose--buffer-data)
    (let ((buf (buffer-expose--bdata-buffer data)))
      ;; might be killed with new kill command
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (face-remap-remove-relative
           (buffer-expose--bdata-cookie data))
          (setq mode-line-format
                (buffer-expose--bdata-mode-line data))
          (setq indicate-buffer-boundaries
                (buffer-expose--bdata-boundaries data))
          (setq header-line-format
                (buffer-expose--bdata-header-line data))
          (setq cursor-type
                (buffer-expose--bdata-cursor data))
          (setq buffer-read-only
                (buffer-expose--bdata-read-only data)))))))


(defun buffer-expose--reset-vars-internal ()
  "Reset internal state tracking vars."
  (setq buffer-expose--next-stack nil
        buffer-expose--prev-stack nil
        buffer-expose--buffer-list nil
        buffer-expose--buffer-data nil
        buffer-expose--last-buffer nil
        buffer-expose--redisable-modes nil
        buffer-expose--reactivate-modes nil
        buffer-expose--reset-variables nil))

(defun buffer-expose--reset-modes ()
  "Reset modes."
  (when (boundp 'fringe-mode)
    (setq fringe-mode (pop buffer-expose-fringe))
    (modify-frame-parameters
     nil  buffer-expose-fringe))

  (dolist (mode buffer-expose--reactivate-modes)
    (funcall mode 1))
  (dolist (mode buffer-expose--redisable-modes)
    (funcall mode -1)))

(defun buffer-expose--reset-vars ()
  "Reset buffer vars."
  (dolist (var buffer-expose--reset-variables)
    (set (car var) (cdr var))))

(defun buffer-expose--window-config ()
  "Return current window config.

Window config is a list of (window . buffer) cells."
  (let* ((w (frame-first-window))
         (conf (list (cons w (window-buffer w)))))
    (while (setq w (buffer-expose--next-window w))
      (push (cons w (window-buffer w))
            conf))
    (nreverse conf)))

(defun buffer-expose--restore-windows (conf)
  "Restore window config CONF.

Window config is a list of (window . buffer) cells."
  (dolist (wb conf)
    (setf (window-buffer (car wb))
          (cdr wb))))

(defun buffer-expose--reset-empty-buffers ()
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'buffer-expose--empty-buffer buf)
      (kill-buffer buf))))

(defun buffer-expose-reset ()
  "Exit overview, restore and reset state."
  (interactive)
  (setq exwm-input-line-mode-passthrough nil)
  (buffer-expose--set-current-buffer-background t)
  (when buffer-expose--cancel-overriding-map-function
    (funcall buffer-expose--cancel-overriding-map-function))
  (set-window-configuration buffer-expose--initial-window-config)
  (buffer-expose--reset-buffers)
  (buffer-expose--reset-empty-buffers)
  (buffer-expose--reset-modes)
  (buffer-expose--reset-vars)
  (buffer-expose--reset-vars-internal))

;; * Entry commands

(defun buffer-expose (&optional max)
  "Expose buffers of `buffer-list'.

If MAX is given it determines the maximum number of windows to
show per page, which defaults to `buffer-expose-max-num-windows'."
  (interactive "P")
  (buffer-expose-show-buffers (buffer-list) max))


(defun buffer-expose-current-mode (&optional max)
  "Expose buffers with mode of current major mode.

MAX is the maximum number of windows to show per page, which
defaults to `buffer-expose-max-num-windows'."
  (interactive "P")
  (buffer-expose-show-buffers
   (buffer-expose--get-mode-buffers major-mode) max))


(defun buffer-expose-major-mode (max mode)
  "Expose buffers with major mode MODE.

MAX is the maximum number of windows to show per page, which
defaults to `buffer-expose-max-num-windows'."
  (interactive
   (list
    current-prefix-arg
    (intern (completing-read
             "Major Mode: "
             (buffer-expose--get-major-modes)))))
  (buffer-expose-show-buffers
   (buffer-expose--get-mode-buffers mode) max))


(defun buffer-expose-stars (&optional max)
  "Expose *special* buffers of `buffer-list'.

If MAX is given it determines the maximum number of windows to
show per page, which defaults to `buffer-expose-max-num-windows'."
  (interactive "P")
  (buffer-expose-show-buffers
   ;; get last buried first
   (nreverse (buffer-list)) max '("\\`[^*]")))


(defun buffer-expose-no-stars (&optional max)
  "Expose buffers of `buffer-list' omitting *special* ones.

If MAX is given it determines the maximum number of windows to
show per page, which defaults to
`buffer-expose-max-num-windows'."
  (interactive "P")
  (buffer-expose-show-buffers
   (buffer-list) max '("\\`\\*")))


(defun buffer-expose-dired-buffers (&optional max)
  "Expose dired buffers of `buffer-list'.

If MAX is given it determines the maximum number of windows to
show per page, which defaults to `buffer-expose-max-num-windows'."
  (interactive "P")
  (buffer-expose-show-buffers
   ;; get last buried first
   (nreverse (buffer-list)) max nil
   (lambda (buf)
     (eq (buffer-local-value 'major-mode buf)
         'dired-mode))))

;; * Grid navigation


(defun buffer-expose--last-to (dir &optional f)
  "Get last window in direction DIR from window F.

F defaults to the currently selected window."
  (let ((w (or f (selected-window)))
        (nw nil))
    (while (setq w (window-in-direction dir w))
      (setq nw w))
    nw))

(defun buffer-expose--first-window-in-col (&optional f)
  "Get first window in column of window F.

F defaults to the currently selected window."
  (buffer-expose--last-to 'above f))

(defun buffer-expose--last-window-in-col (&optional f)
  "Get first window in column of window F.

F defaults to the currently selected window."
  (buffer-expose--last-to 'below f))

(defun buffer-expose--first-window-in-row (&optional f)
  "Get first window in row of window F.

F defaults to the currently selected window."
  (buffer-expose--last-to 'left f))

(defun buffer-expose--last-window-in-row (&optional f)
  "Get last window in row of window F.

F defaults to the currently selected window."
  (buffer-expose--last-to 'right f))

(defun buffer-expose--get-current-row (&optional f)
  "Get row of window F.

F defaults to the currently selected window."
  (let ((w (or f (selected-window)))
        (nw 0))
    (while (setq w (window-in-direction 'above w))
      (cl-incf nw))
    nw))

(defun buffer-expose--get-window-in-row (n &optional f)
  "Get window in Nth row form window F.

F defaults to the first window of the overview."
  (let ((w (or f (frame-first-window))))
    (while (and (> n 0)
                (setq w (window-in-direction 'below w))
                (cl-decf n)))
    w))

(defun buffer-expose--next-window (&optional f)
  "Get next window for window F.

F defaults to the currently selected window."
  (let ((f (or f (selected-window)))
        (w nil))
    (or (window-in-direction 'right f)
        (when (setq w (window-in-direction 'below f))
          (buffer-expose--first-window-in-row w)))))

(defun buffer-expose--prev-window (&optional f)
  "Get previous window for window F.

F defaults to the currently selected window."
  (let ((f (or f (selected-window)))
        (w nil)
        (nw nil))
    (or (window-in-direction 'left f)
        (when (setq w (window-in-direction 'above f))
          (while (setq w (window-in-direction 'right w))
            (setq nw w))
          nw))))

(defun buffer-expose--last-window ()
  "Get last window of overview."
  (let ((w (selected-window))
        (nw nil))
    (while (setq w (or (window-in-direction 'right w)
                       (window-in-direction 'below w)))
      (setq nw w))
    nw))

(defun buffer-expose-switch-to-buffer ()
  "Switch to buffer using `buffer-expose-switch-to-buffer-func'."
  (interactive)
  (funcall buffer-expose--cancel-overriding-map-function)
  (let (buf)
    (unwind-protect
        (progn
          (buffer-expose--set-current-buffer-background t)
          (setq buf
                (call-interactively buffer-expose-switch-to-buffer-func)))
      (if (not buf)
          (progn (buffer-expose--set-current-buffer-background)
                 (buffer-expose--init-map))
        (buffer-expose-reset)
        (switch-to-buffer buf)))))

(defun buffer-expose-up-window ()
  "Switch to window above."
  (interactive)
  (let ((w (window-in-direction 'above)))
    (if w (buffer-expose--select-window w)
      (if buffer-expose-wrap-vertically
          (buffer-expose--select-window (buffer-expose--last-window-in-col))
        (user-error "No window above current window")))))

(defun buffer-expose-down-window ()
  "Switch to window below."
  (interactive)
  (let ((w (window-in-direction 'below)))
    (if w (buffer-expose--select-window w)
      (if buffer-expose-wrap-vertically
          (buffer-expose--select-window (buffer-expose--first-window-in-col))
        (user-error "No window below current window")))))

(defun buffer-expose-left-window ()
  "Switch to window at left side."
  (interactive)
  (let ((w (window-in-direction 'left)))
    (if w (buffer-expose--select-window w)
      (let ((row (buffer-expose--get-current-row)))
        (buffer-expose-prev-page)
        (buffer-expose--select-window
         (buffer-expose--get-window-in-row
          row
          (buffer-expose--last-window-in-row (frame-first-window))))))))

(defun buffer-expose-right-window ()
  "Switch to window at right side."
  (interactive)
  (let ((w (window-in-direction 'right)))
    (if w (buffer-expose--select-window w)
      (let ((row (buffer-expose--get-current-row)))
        (buffer-expose-next-page)
        (buffer-expose--select-window
         (buffer-expose--get-window-in-row row))))))

(defun buffer-expose-next-window ()
  "Switch to next window."
  (interactive)
  (let ((w (buffer-expose--next-window)))
    (if w (buffer-expose--select-window w)
      (buffer-expose-next-page))))

(defun buffer-expose-prev-window ()
  "Switch to previous window."
  (interactive)
  (let ((w (buffer-expose--prev-window)))
    (if w (buffer-expose--select-window w)
      (buffer-expose-prev-page)
      (buffer-expose--select-window (buffer-expose--last-window)))))

(defun buffer-expose-first-window-in-row ()
  "Switch to first window in current row."
  (interactive)
  (let ((w (buffer-expose--first-window-in-row)))
    (when w (buffer-expose--select-window w))))

(defun buffer-expose-last-window-in-row ()
  "Switch to last window in current row."
  (interactive)
  (let ((w (buffer-expose--last-window-in-row)))
    (when w (buffer-expose--select-window w))))

(defun buffer-expose-last-window ()
  "Select last window of overview."
  (interactive)
  (buffer-expose--select-window (buffer-expose--last-window)))

(defun buffer-expose-first-window ()
  "Select first window of overview."
  (interactive)
  (buffer-expose--select-window (frame-first-window)))


(defun buffer-expose-handle-mouse (e)
  "Chosse clicked window using event E."
  (interactive "e")
  (buffer-expose--select-window (posn-window (event-start e)))
  (buffer-expose-choose))

(defun buffer-expose-aw-switch-to-window (w)
  "Switch to choosen window W."
  (buffer-expose--set-current-buffer-background t)
  (funcall #'aw-switch-to-window w)
  (buffer-expose-choose))

(defun buffer-expose-ace-handler (char)
  "Execute buffer-expose action for CHAR."
  (cond ((memq char '(27 ?\C-g ?,))
         ;; exit silently
         (throw 'done 'exit))
        ((mouse-event-p char)
         (signal 'user-error (list "Mouse event not handled" char)))
        (t
         (require 'edmacro)
         (let* ((key (kbd (edmacro-format-keys (vector char))))
                (cmd (or (lookup-key buffer-expose-exit-map key)
                         (lookup-key buffer-expose-grid-map key))))
           (if cmd
               (progn (call-interactively cmd)
                      (throw 'done 'exit))
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char)))))))

(defun buffer-expose-ace-window ()
  "Choose a window with ‘ace-window’."
  (interactive)
  (if (not (require 'ace-window nil t))
      (user-error "Ace Windows not found")
    (let* ((buffer-expose--ace-p t)
           (aw-keys buffer-expose-aw-keys)
           (aw-background nil)
           (aw-ignored-buffers nil)
           (avy-dispatch-alist nil)
           (aw-dispatch-function #'buffer-expose-ace-handler)
           (foreground (face-attribute 'aw-leading-char-face :foreground)))
      (cl-letf (((symbol-function #'aw--lead-overlay)
                 #'ignore))
      (unwind-protect
            (progn (set-face-attribute 'aw-leading-char-face
                                       nil
                                       :foreground
                                       (face-attribute 'default :background))
                   (aw-update)
                   (aw-select " " #'buffer-expose-aw-switch-to-window))
          (set-face-attribute 'aw-leading-char-face
                              nil
                              :foreground
                              foreground))))))

(defun buffer-expose-next-page ()
  "Page to next view."
  (interactive)
  (when (or buffer-expose--prev-stack
            buffer-expose--buffer-list)
    (push (buffer-expose--window-config) buffer-expose--next-stack))
  (if buffer-expose--prev-stack
      (progn (buffer-expose--restore-windows
              (pop buffer-expose--prev-stack))
             (buffer-expose--select-window (frame-first-window))
             (when buffer-expose-auto-init-aw
               (buffer-expose-ace-window)))
    (if buffer-expose--buffer-list
        (progn
          (buffer-expose-fill-grid)
          ;; update the new window for highlighting
          (buffer-expose--select-window (frame-first-window))
          (when buffer-expose-auto-init-aw
            (buffer-expose-ace-window)))
      (error "No next view available"))))

(defun buffer-expose-prev-page ()
  "Page to previous view."
  (interactive)
  (if buffer-expose--next-stack
      (progn
        (push (buffer-expose--window-config)
              buffer-expose--prev-stack)
        (buffer-expose--restore-windows (pop buffer-expose--next-stack))
        ;; for consistency with next-page make sure it behaves the same
        (buffer-expose--select-window (frame-first-window))
        (when buffer-expose-auto-init-aw
          (buffer-expose-ace-window)))
    (error "No previous view available")))

(defun buffer-expose-kill-buffer ()
  "Kill currently selected buffer."
  (interactive)
  (let ((buf (window-buffer))
        (w (get-buffer-window)))
    (let ((overriding-terminal-local-map nil))
      (when (kill-buffer buf)
        (setq buffer-expose--selected-cookie nil)
        (setf (window-buffer w)
              (buffer-expose--create-empty-buffer))
        (buffer-expose--select-window
         (or (window-in-direction 'right)
             (window-in-direction 'below)
             (window-in-direction 'left)
             (selected-window)))))))

(defun buffer-expose-choose ()
  "Choose buffer and exit overview."
  (interactive)
  (funcall buffer-expose-choose-action-func (current-buffer)))


(provide 'buffer-expose)
;;; buffer-expose.el ends here
