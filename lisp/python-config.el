(use-package python-mode
  :config
  (setq python-interpreter "ipython")
  :bind
  (:map python-mode-map
        ("M-q" . ruff-format-region)))

;; (use-package pet
;;   :straight t
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package uv-menu
  :straight (uv-menu
	     :type git
	     :host github
	     :repo "pizzatorque/uv-menu"
	     :files ("*.el" "data")))

(use-package ruff-format :straight t)

(provide 'python-config)
