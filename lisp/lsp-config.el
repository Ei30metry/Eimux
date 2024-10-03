;;; lsp-config.el --- Description -*- lexical-binding: t; -*-
(setq gc-cons-threshold 100000000)
(use-package eglot
  :ensure nil
  :commands eglot
  :bind
  ("C-c C-e C-e" . eglot)
  (:map eglot-mode-map
  ("C-c C-s" . consult-eglot-symbols)
  ("C-c C-." . eldoc)
  ("C-c C-e C-f" . consult-flymake)
  ("<C-m> C-n" . flymake-goto-next-error)
  ("<C-m> C-p" . flymake-goto-prev-error)
  ("C-c C-a C-c" . eglot-code-actions)
  ("C-c C-e C-t" . eglot-find-typeDefinition)
  ("C-c C-a C-i" . eglot-code-action-inline)
  ("C-c C-a C-e" . eglot-code-action-extract)
  ("C-c C-a C-o" . eglot-code-action-organize-imports)
  ("C-c C-a C-r" . eglot-code-action-rewrite)
  ("C-c C-a C-a" . eglot-code-action-quickfix)
  ("C-c C-e C-r" . eglot-rename)
  ("C-c C-e C-s C-r" . eglot-reconnect)
  ("C-c C-e C-s C-s" . eglot-shutdown)
  ("C-c C-e C-s C-a" . eglot-shutdown-all))
  :config
  (setq-default eglot-workspace-configuration
        '((haskell (plugin (stan (globalOn . :json-false))))))
  (setq eglot-confirm-server-initiated-edits nil))

(use-package eglot-x
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x"
	     :files ("*.el"))
  :config
  :after rustic
  (eglot-x-setup))

(provide 'lsp-config)
