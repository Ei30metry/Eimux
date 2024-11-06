;;; document-viewing-config.el --- Description -*- lexical-binding: t; -*-

(defun reading-setup ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (face-remap-add-relative 'variable-pitch :family "Canela Text"
                                           :height 1.2)
   (text-scale-set 1))

(use-package olivetti :straight t)

(use-package nov
   :straight t
   :demand t
   :bind
   (:map nov-mode-map
   ("j" . osx-dictionary-search-word-at-point))
   :mode
   (("\\.epub\\'" . nov-mode))
   :hook
   (nov-mode . olivetti-mode)
   (nov-mode . reading-setup))

(use-package pdf-tools
  :demand t
  :bind
  (:map pdf-view-mode-map ("g" . revert-buffer-quick)
                          ("M-s o" . occur)
                          ("<C-m> C-o" . occur)
                          ("o" . pdf-outline)
                          ("M-g M-g" . pdf-view-goto-page))
  :mode
  (("\\.pdf\\'" . pdf-view-mode))
  :config
  (auto-revert-mode 1)
  (setq auto-revert-verbose nil
        pdf-view-use-scaling t)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode))

(defun ei30/pdf-view-dark-mode ()
  (interactive)
  (message "TODO"))

(use-package djvu :straight t)

(use-package djvu3
  :straight
    (djvu3
     :type git
     :host github
     :repo "dalanicolai/djvu3"
     :files (".el")))

(provide 'document-viewing-config)
