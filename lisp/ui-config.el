;;; ui-config.el --- Description -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t
      inhibit-startup-message t)

(winner-mode 1)
(setq display-line-numbers-type 't
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
  :config (ns-auto-titlebar-mode 1))

(use-package mood-line
  :straight t)

(column-number-mode 1)

(add-hook 'prog-mode-hook #'(lambda () (display-line-numbers-mode 1)))

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
   (doom-themes-org-config))

(load-theme 'doom-1337 t)

(provide 'ui-config)
