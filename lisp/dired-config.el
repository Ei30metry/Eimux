(use-package diredfl
  :straight t
  :init
  (diredfl-global-mode))

(global-unset-key (kbd "C-x D"))

(use-package dired-preview
  :straight t
  :config
  (setq dired-preview-delay 0.4
        dired-preview-max-size (expt 2 20)
        dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "\\)")))

(use-package dwim-shell-command :straight t)

(use-package dired
  :bind
  ("C-x D" . dired-jump)
  (:map dired-mode-map
        ("C-j" . dired-jump)
        ("P"   . dired-preview-mode)
        ("e"   . dired-toggle-read-only))
  :config
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  (require 'dired-x)
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-listing-switches "-alFh"))

(use-package disk-usage :straight t)

(use-package dired-narrow :straight t)

(use-package dired-atool :straight t)

;; Default values for demo purposes


(provide 'dired-config)
