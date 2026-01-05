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
         ("j" . osx-dictionary-search-word-at-point)
         ("<mouse-3>" . osx-dictionary-search-word-at-point))
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
        ("j" . osx-dictionary-search-input)
        ("M-g M-g" . pdf-view-goto-page))
  :mode
  (("\\.pdf\\'" . pdf-view-mode))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-view-continuous t)
  (pdf-tools-handle-upgrades nil)
  (pdf-tools-enabled-modes '(pdf-view-dark-minor-mode
                             pdf-history-minor-mode
                             pdf-isearch-minor-mode
                             pdf-links-minor-mode
                             pdf-outline-minor-mode
                             pdf-misc-size-indication-minor-mode
                             pdf-misc-context-menu-minor-mode
                             pdf-annot-minor-mode
                             pdf-sync-minor-mode
                             pdf-cache-prefetch-minor-mode))
  :config
  (setq auto-revert-verbose nil
        pdf-view-use-scaling t)
  (pdf-tools-install)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode))


(defun artin/add-current-word-to-new-words (word)
  "Adds word to the new words section of the book that is currently being read."
  (message "Not implemented yet"))


(use-package pdf-history
  :ensure nil
  :hook ((pdf-view-mode . pdf-history-minor-mode)))

(use-package pdf-links
  :hook ((pdf-view-mode . pdf-links-minor-mode)))

(use-package pdf-outline
  :hook ((pdf-view-mode . pdf-outline-minor-mode)))

(use-package pdf-isearch
  :hook ((pdf-view-mode . pdf-isearch-minor-mode)))

(use-package pdf-annot
  :hook ((pdf-view-mode . pdf-annot-minor-mode)))

(use-package pdf-sync
  :hook ((pdf-view-mode . pdf-sync-minor-mode)))

(use-package pdf-cache
  :hook ((pdf-view-mode . pdf-cache-prefetch-minor-mode)))

(use-package djvu :straight t)

(use-package djvu3
  :straight
    (djvu3
     :type git
     :host github
     :repo "dalanicolai/djvu3"
     :files (".el")))

(use-package shrface :straight t)

(use-package calibredb
  :straight t
  :config
  (setq calibredb-root-dir "~/Books")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Books" (name . "Books")))))

(provide 'document-viewing-config)
