(use-package proof-general :straight t)

(use-package company-coq :straight t)

(add-hook 'coq-mode-hook #'company-coq-mode)

(provide 'coq-config)
