(use-package diredfl
  :straight t
  :init
  (diredfl-global-mode))

(use-package dired
  :bind
  (:map dired-mode-map
   ("C-j" . dired-jump))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(provide 'dired-config)
