;;; Basic rspec setup
(require-package 'rspec-mode)

(require 'rspec-mode)
(require 'init-powershop-markets)

(setq rspec-spec-command " bundle exec rspec")
(setq rspec-use-bundler-when-possible nil)
(setq rspec-use-spring-when-possible nil)
(setq rspec-primary-source-dirs '("app" "lib" "core" "nz" "au" "uk"))
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
         (format "%s %s"
                 (cond ((equal market "psnz") "RETAILER=psnz COUNTRY=nz RETAIL_BRAND=powershop")
                       ((equal market "psau") "RETAILER=psau COUNTRY=au RETAIL_BRAND=powershop")
                       ((equal market "psuk") "RETAILER=psuk COUNTRY=uk RETAIL_BRAND=powershop")
                       ((equal market "merx") "RETAILER=merx COUNTRY=nz RETAIL_BRAND=meridian" ))
                 rspec-spec-command)))
    (funcall rspec-function)))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(provide 'init-rspec-mode)
