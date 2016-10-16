;; Thanks to DJCB for his blog (emacs-fu.blogspot.com).

;; Myself
(setq-default user-full-name "Vincent Boucheny")
(setq-default user-mail-address "vincent.boucheny@powerhop.co.nz")

;;;;
;; Import configuration files.
;;;;

;; (load "keybindings.el")
;; (load "hooks.el")
;; (load "ruby.el")

;;;;
;; Global mode configuration
;;;;

;; Default font
;;(set-frame-font "-misc-fixed-medium-r-semicondensed--13-*-75-75-c-60-iso8859-1")

;; Cool frame title with currently edited buffer name
(setq frame-title-format "%b")

;; File time-stamping
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for time-stamp.
 time-stamp-format "%02d-%02m-%04y %02H:%02M:%02S") ; date format

;; 'y' instead of "yes", 'n' instead of "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; For maximizing on startup.
(require 'frame-cmds)
(maximize-frame)

;; Highlight the current line. Set a custom face, so we can recognize
;; from the normal marking (selection).
(defface hl-line '((t (:background "DimGray")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
;; Turn it on for all modes by default
(global-hl-line-mode nil)

;; Show trailing white spaces

(setq-default
 gnus-inhibit-startup-message t ; Don't show startup messages
 inhibit-startup-message t ; Don't show startup messages
 inhibit-startup-echo-area-message t ; Don't show startup messages
 search-highlight t ; Highlight when searching...
 query-replace-highlight t ; ...and replacing
 show-trailing-whitespace t ; Show trailing white spaces
 echo-keystrokes 0.1
 selection-coding-system 'compound-text-with-extensions
 display-time-24hr-format t
 require-final-newline t
 case-fold-search t ; case-insensitive search
 kill-whole-line t ; C-k kills the line _and_ the \n
 )

;; Enable disabled command
(put 'upcase-region 'disabled nil)    ;; same as M-u but on whole regions C-x C-u
(put 'downcase-region 'disabled nil)  ;; same as M-l but on whole regions C-x C-l

(column-number-mode t) ; show column numbers
(transient-mark-mode t) ; make the current 'selection' visible
(delete-selection-mode t) ; delete the selection area with a keypress
(blink-cursor-mode -1)	; don't blink the cursor
(mouse-avoidance-mode 'animate) ; Drive out the mouse when it's too near to the cursor
(size-indication-mode) ; Show approx buffer size in modeline
(global-font-lock-mode t) ; Syntax coloration
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;(display-time)
;;(normal-erase-is-backspace-mode 1)
(global-auto-revert-mode 1)
(auto-fill-mode 1)
(projectile-mode t)

;; Striping ^M from files, Stolen from BradfordHolcombe.emacs (its tiny, yet handy):
(fset 'dem [?\M-< ?\M-% ?\C-q ?\C-m return return ?! ?\M-<])

;; Parenthesis
(show-paren-mode t) ; turn paren-mode on
(setq show-paren-delay 1 ; how long to wait?
      show-paren-style 'mixed) ; alternatives are 'parenthesis' and 'mixed'
(set-face-foreground 'show-paren-mismatch-face "red")
(set-face-foreground 'show-paren-match-face "green")

(autoload 'linum-mode "linum" "toggle line numbers on/off" t)

(provide 'init-local)
