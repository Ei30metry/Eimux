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
        org-log-done 'time
        org-insert-heading-respect-content t
        org-treat-insert-todo-heading-as-state-change t
        org-hide-emphasis-markers t
        org-return-follows-link t
        org-src-tab-acts-natively nil
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))
        org-agenda-files '("~/Agenda/tasks.org"))
  :hook
  (org-agenda-mode . (lambda () (visual-line-mode -1) (toggle-truncate-lines 1))))

(require 'org-tempo)
(setq org-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("py" . "src python")
        ("sq" . "src sql")
        ("hs" . "src haskell")
        ("lt"  . "src latex")
        ("rs" . "src rust")
        ("c"  . "src c")
        ("t" . "src txt")
        ("o" . "src ott")))

(use-package org-books
 :straight t
 :after org
 :config
 (setq org-books-file "~/Agenda/books.org"))

(setq org-capture-templates
      '(("T" "Planned" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %^t\nDEADLINE: %^t")
        ("t" "Today" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %t")
        ;; ("m" "Tomorrow" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %(t+1)\nDEADLINE: %(t+1)") TODO: Fix this
        ("l" "Process later" entry (file+headline "tasks.org" "Inbox") "* TODO %?")
        ("p" "Project")
        ("ps" "Specification" entry (file+olp "projects/specification.org" "Tasks" "To Plan") "* TODO %?")
        ("pg" "GHC" entry (file+headline "projects/ghc.org" "Tasks") "* TODO %?")
        ("c" "Config")
        ("ce" "Emacs" entry (file+headline "config/emacs-config.org" "Tasks") "* TODO %?\n")
        ("cn" "Nix" entry (file+headline "config/nix.org" "Tasks") "* TODO %?\n  %i")))

(global-unset-key (kbd "C-x C-r"))

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

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))

(provide 'org-config)
