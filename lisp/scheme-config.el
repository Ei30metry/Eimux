(use-package geiser
  :straight t
  :hook
  (geiser-repl-mode . smartparens-strict-mode))

(use-package geiser-chez
  :straight t)

(use-package scheme-complete :straight )

(use-package macrostep-geiser
  :straight t
  :after geiser-mode
  :config
  (geiser-mode . macrostep-geiser-setup))

(provide 'scheme-config)
