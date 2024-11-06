(use-package diredfl
  :straight t
  :init
  (diredfl-global-mode))

(global-unset-key (kbd "C-x D"))

(use-package dired
  :bind
  ("C-x D" . dired-jump)
  (:map dired-mode-map
        ("C-j" . dired-jump))
  :config
  (require 'dired-x)
  (setq dired-kill-when-opening-new-dired-buffer t))

(provide 'dired-config)
