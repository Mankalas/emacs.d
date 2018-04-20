;;; Basic rspec setup
(require-package 'rspec-mode)

(require 'rspec-mode)
(require 'init-powershop-markets)

(setq rspec-spec-command " bundle exec spring rspec")
(setq rspec-use-bundler-when-possible nil)
(setq rspec-use-spring-when-possible nil)
(setq rspec-primary-source-dirs '("app" "lib" "core" "psnz" "psau" "psuk"))
(setq rspec-command-options "--color --format documentation")

(define-key rspec-verifiable-mode-keymap (kbd "v") 'powershop-rspec-verify)
(define-key rspec-verifiable-mode-keymap (kbd "r") 'powershop-rspec-rerun)
(define-key rspec-verifiable-mode-keymap (kbd "f") 'powershop-rspec-run-last-failed)
(define-key rspec-mode-keymap (kbd "s") 'powershop-rspec-verify-single)


(defun powershop-rspec-verify (market)
  "Run spec for the current buffer in the specified MARKET."
  (interactive
   (list (powershop-read-market)))
  (powershop-override-rspec-function 'rspec-verify))

(defun powershop-rspec-verify-single (market)
  "Run spec for the current example in the specified MARKET."
  (interactive
   (list (powershop-read-market)))
  (powershop-override-rspec-function 'rspec-verify-single))

(defun powershop-rspec-rerun (market)
  "Re-run the last RSpec invocation in the specified MARKET."
  (interactive
   (list (powershop-read-market)))
  (powershop-override-rspec-function 'rspec-rerun))

(defun powershop-rspec-run-last-failed (market)
  "Run just the specs that failed during the last invocation in the specified MARKET."
  (interactive
   (list (powershop-read-market)))
  (powershop-override-rspec-function 'rspec-run-last-failed))

(defun powershop-override-rspec-function (rspec-function)
  (let ((rspec-spec-command
         (format "PS_MARKET=%s bundle exec spring rspec" market)))
    (funcall rspec-function)))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(provide 'init-rspec-mode)
