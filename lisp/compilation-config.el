;; compilation-config --- Description -*- lexical-binding: t; -*-

(use-package compile
  :bind
  (:map compilation-mode-map
   ("c" . project-compile))
  :hook (compilation-filter . comint-ansi-color-process-output)
  :config
  (setq compilation-always-kill t))

(use-package imake :straight t)

(use-package rmsbolt
  :straight t)

(global-set-key (kbd "<C-m> C-n") 'next-error)
(global-set-key (kbd "<C-m> C-p") 'previous-error)

(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)

(provide 'compilation-config)
