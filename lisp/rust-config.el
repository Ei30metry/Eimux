;;; rust-config.el --- Description -*- lexical-binding: t; -*-

(use-package rust-mode
  :straight t)

(use-package rustic
  :straight t
  :config
  (setq rustic-lsp-client 'eglot))

(use-package cargo
  :straight t)

(provide 'rust-config)
