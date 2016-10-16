;; Myself
(setq-default user-full-name "Vincent Boucheny")
(setq-default user-mail-address "vincent.boucheny@powerhop.co.nz")

;; (load "keybindings.el")
;; (load "hooks.el")
;; (load "ruby.el")

;;(set-frame-font "-misc-fixed-medium-r-semicondensed--13-*-75-75-c-60-iso8859-1")

;; Striping ^M from files, Stolen from BradfordHolcombe.emacs (its tiny, yet handy):
(fset 'dem [?\M-< ?\M-% ?\C-q ?\C-m return return ?! ?\M-<])

(provide 'init-local)
