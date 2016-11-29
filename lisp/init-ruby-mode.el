;;; Basic ruby setup
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))

(require-package 'rspec-mode)


;;; Inferior ruby
(require-package 'inf-ruby)



;;; Ruby compilation
(require-package 'ruby-compilation)

(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m [S-f7] 'ruby-compilation-this-buffer)
    (define-key m [f7] 'ruby-compilation-this-test)))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))
(after-load 'company
  (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook html-erb-mode-hook haml-mode))
    (add-hook hook
              (lambda () (sanityinc/local-push-company-backend 'company-robe)))))



;; Customise highlight-symbol to not highlight do/end/class/def etc.
(defun sanityinc/suppress-ruby-mode-keyword-highlights ()
  "Suppress highlight-symbol for do/end etc."
  (set (make-local-variable 'highlight-symbol-ignore-list)
       (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))
(add-hook 'ruby-mode-hook 'sanityinc/suppress-ruby-mode-keyword-highlights)



;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)



(require-package 'goto-gem)


(require-package 'bundler)


;;; YAML

(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'"))



;;; ERB
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . web-mode))
(require 'web-mode)
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'init-ruby-mode)
