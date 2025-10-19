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
        org-agenda-files '("~/Agenda/tasks.org")
        org-extend-today-until 3
        org-use-effective-time t)
  :hook
  (org-agenda-mode . (lambda ()
                       (visual-line-mode -1)
                       (toggle-truncate-lines 1))))

(defun echo-area-tooltips ()
  "Show tooltips in the echo area automatically for current buffer."
  (setq-local help-at-pt-display-when-idle t
              help-at-pt-timer-delay 0)
  (help-at-pt-cancel-timer)
  (help-at-pt-set-timer))

(defun org-heading-remove-scheduled ()
  "Remove the SCHEDULED property"
  (interactive)
  (cl-flet ((remove-schedule ()
              (save-excursion
                (let ((keyword (org-entry-get (point) "SCHEDULED")))
                  (when keyword
                    (org-remove-timestamp-with-keyword org-scheduled-string))))))
    (if (region-active-p)
        (org-map-entries #'remove-schedule)
      (remove-schedule))))

(defun org-heading-remove-deadline ()
  "Remove the DEADLINE property"
  (interactive)
  (cl-flet ((remove-deadline ()
              (save-excursion
                (let ((keyword (org-entry-get (point) "DEADLINE")))
                  (when keyword
                    (org-remove-timestamp-with-keyword org-deadline-string))))))
    (if (region-active-p)
        (org-map-entries #'remove-deadline)
      (remove-deadline))))

(defun org-heading-remove-scheduled-and-deadline ()
  "Remove the SCHEDULED and DEADLINE properties"
  (interactive)
  (org-heading-remove-scheduled)
  (org-heading-remove-deadline))


(require 'org-tempo)
(setq org-structure-template-alist
      '(("el" . "src emacs-lisp")
        ("cl" . "src common-lisp")
        ("py" . "src python")
        ("sq" . "src sql")
        ("hs" . "src haskell")
        ("lt" . "src latex")
        ("rs" . "src rust")
        ("c"  . "src c")
        ("t"  . "src txt")
        ("o"  . "src ott")
        ("ss" . "src scheme")))

(use-package org-books
 :straight t
 :after org
 :config
 (setq org-books-file "~/Agenda/books.org"))

(setq org-capture-templates
      '(("l" "Process later" entry (file+headline "tasks.org" "To process") "* TODO %?")
        ("s" "Planned" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %^t")
        ("t" "Today" entry (file+headline "tasks.org" "Planned") "* TODO %?\nSCHEDULED: %t")
        ("m" "Tomorrow" entry (file+headline "tasks.org" "Planned")
         "* TODO %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d>\" (time-add (current-time) (days-to-time 1)))")))

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

(use-package org-roam-ql
  :straight t)

(use-package org-super-agenda
  :straight t)

(use-package org-pomodoro :straight t)

;; (use-package org-supertag :straight t)

(use-package org-workbench
  :straight (:host github :repo "yibie/org-workbench")
  :after org-roam ; or org-roam, org-brain, etc.
  :config
  (org-workbench-setup))

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

;; (use-package websocket-bridge
;;   :straight
;;   (websocket-bridge
;;    :repo "ginqi7/websocket-bridge"
;;    :host github
;;    :type git
;;    :files ("*.el")))

;; (use-package org-reminders
;;   :straight (org-reminders
;;              :type git
;;              :host github
;;              :repo "ginqi7/org-reminders"
;;              :files ("*.el"))
;;   :config
;;   (setq org-reminders-sync-file "/Users/artin/Agenda/tasks.org"))

;; (use-package org-reminders-cli)

(provide 'org-config)
