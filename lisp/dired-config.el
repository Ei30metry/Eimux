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
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (require 'dired-x)
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-alFh"))

(use-package disk-usage :straight t)

(provide 'dired-config)
