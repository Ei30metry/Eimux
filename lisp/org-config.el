;;; org-config.el --- Description -*- lexical-binding: t; -*-
(use-package org
  :straight t
  :demand t
  :bind
  ("C-x A" . org-agenda)
  ("C-," . popper-toggle)
  ("C-S-c" . org-capture)
  (:map org-mode-map ("C-S-c" . org-capture) ("C-," . nil) ("C-'" . nil))
  :config
  (setq org-startup-indented t
        org-directory "~/Agenda"
        org-log-into-drawer t
        org-treat-insert-todo-heading-as-state-change t
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-src-tab-acts-natively nil
        org-agenda-files '("~/Agenda/tasks.org"  "~/Agenda/projects/specification.org"))
  :hook
  (org-agenda-mode . (lambda () (visual-line-mode -1) (toggle-truncate-lines 1))))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("py" . "src python")
        ("sq" . "src sql")
        ("hs" . "src haskell")
        ("t" . "src tex")
        ("rs" . "src rust")
        ("c"  . "src c")
        ("tx" . "src txt")
        ("o" . "src ott")))

(use-package org-books
 :straight t
 :after org
 :config
 (setq org-books-file "~/Agenda/books.org"))

(setq org-capture-templates
     '(("t" "Task")
       ("tt" "Planned" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %^t\nDEADLINE: %^t")
       ("tT" "Today" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %t\nDEADLINE: %t")
       ("tl" "Process later" entry (file+headline "tasks.org" "Inbox") "* TODO %?")
       ("tp" "Project")
       ("tps" "Specification" entry (file+headline "projects/specification.org" "Tasks") "* TODO %?")
       ("tpg" "GHC" entry (file+headline "projects/ghc.org" "Tasks") "* TODO %?")
       ("tc" "Config")
       ("tce" "Emacs" entry (file+headline "config/emacs-config.org" "Tasks") "* TODO %?\n")
       ("tcn" "Nix" entry (file+headline "config/nix.org" "Tasks") "* TODO %?\n  %i")
       ("a" "Ask" entry (file+headline "projects/specification.org" "QUESTIONS") "* QUESTION %?\n")))

(use-package org-roam
  :straight t
  :demand t
  :bind
  ("C-x C-r C-r"     . org-roam-capture)
  ("C-x C-r C-t"     . org-roam-dailies-capture-today)
  ("C-x C-r C-j t"   . org-roam-dailies-goto-today)
  ("C-x C-r w"       . org-roam-refile)
  ("C-x C-r C-j y"   . org-roam-dailies-goto-yesterday)
  ("C-x C-r C-j C-d" . org-roam-dailies-find-directory)
  ("C-x C-r C-j n"   . org-roam-dailies-goto-next-note)
  ("C-x C-r C-j p"   . org-roam-dailies-goto-previous-note)
  ("C-x C-r C-j d"   . org-roam-dailies-goto-date)
  ("C-x C-r b"       . org-roam-buffer-display-dedicated)
  ("C-x C-r C-i r"   . org-roam-ref-add)
  ("C-x C-r C-i t"   . org-roam-tag-add)
  ("C-x C-r C-i a"   . org-roam-alias-add)
  ("C-x C-r C-i n"   . org-roam-node-insert)
  ("C-x C-r C-f"     . org-roam-node-find)
  (:map org-roam-mode-map ("M-." . org-roam-ref-find))
  :config
  (setq org-roam-directory "~/Roam"
        org-roam-db-autosync-mode t))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :bind
    ("C-x C-r C-u" . org-roam-ui-open)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-roam-bibtex
  :straight t)

(use-package toc-org
 :straight t
 :after org
 :hook
 (org-mode . toc-org-mode))

(provide 'org-config)
