;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'origami)
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)
    (define-key origami-mode-map (kbd "C-c <left>") 'origami-close-node)
    (define-key origami-mode-map (kbd "C-c <right>") 'origami-open-node)
    ))


(provide 'init-folding)
;;; init-folding.el ends here
