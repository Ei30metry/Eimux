(use-package no-littering :straight t)

(setq custom-file (make-temp-name "/tmp/"))

(use-package exec-path-from-shell
 :straight t
 :config
 (setq exec-path-from-shell-arguments nil)
 (exec-path-from-shell-initialize))

(use-package envrc
  :straight t
  :hook
  (after-init . envrc-global-mode))

(provide 'env-stuff)
