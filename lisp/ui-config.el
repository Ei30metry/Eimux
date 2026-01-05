;;; ui-config.el --- Description -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(winner-mode 1)

(setq display-line-numbers-type 'nil
      scroll-conservatively 101)
   (if (display-graphic-p)
       (menu-bar-mode 1)
       (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)
(global-visual-line-mode 1)

(use-package ns-auto-titlebar
  :straight t
  :config
  (ns-auto-titlebar-mode 1))

(column-number-mode 1)

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (bound-and-true-p treesit-font-lock-feature-list)
              (setq-local jit-lock-defer-time 0.03))))

(require 'hl-line)

(setq initial-scratch-message nil)

(setq-default show-trailing-whitespace nil)

(setq-mode-local show-trailing-whitespace t)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-1337 t))

(use-package autothemer :straight t)

(use-package kanagawa-themes :straight t)

(use-package oxocarbon-emacs
  :straight (oxocarbon-theme
             :type git
             :host github
             :repo "konrad1977/oxocarbon-emacs"
             :files ("*.el")))

(use-package stimmung-themes :straight t)

(use-package kaolin-themes :straight t)

(use-package ef-themes :straight t)

(use-package humanoid-themes :straight t)

(use-package flyover
  :straight (flyover
             :type git
             :host github
             :repo "konrad1977/flyover"
             :files ("*.el")))

(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-support-imenu t
        doom-modeline-height 22
        doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-window-width-limit 85
        doom-modeline-spc-face-overrides nil
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-lsp-icon t
        doom-modeline-unicode-fallback nil
        doom-modeline-buffer-name t
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-column-zero-based t
        doom-modeline-percent-position '(-3 "%p")
        doom-modeline-position-line-format '("L%l")
        doom-modeline-position-column-format '("C%c")
        doom-modeline-position-column-line-format "%l:%c"
        doom-modeline-minor-modes t
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-total-line-number t
        doom-modeline-vcs-icon nil
        doom-modeline-vcs-max-length 15
        doom-modeline-vcs-display-function #'doom-modeline-vcs-name
        doom-modeline-vcs-state-faces-alist
        '((needs-update . (doom-modeline-warning bold))
          (removed . (doom-modeline-urgent bold))
          (conflict . (doom-modeline-urgent bold))
          (unregistered . (doom-modeline-urgent bold)))
        doom-modeline-check-icon t
        doom-modeline-check-simple-format nil
        doom-modeline-number-limit 99
        doom-modeline-project-name t
        doom-modeline-workspace-name t
        doom-modeline-lsp t
        doom-modeline-display-misc-in-all-mode-lines t
        doom-modeline-buffer-file-name-function #'identity
        doom-modeline-buffer-file-truename-function #'identity
        doom-modeline-env-load-string "..."
        doom-modeline-before-update-env-hook nil
        doom-modeline-after-update-env-hook nil))

(provide 'ui-config)
