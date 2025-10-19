;;; code-navigation-config.el --- Description -*- lexical-binding: t; -*-

(use-package imenu-anywhere :straight t)

(use-package imenu-list :straight t)

(use-package dumb-jump
    :straight t
    :demand t
    :config
    (setq dumb-jump-force-searcher 'ag))

(use-package xref
  :straight t
  :config
  (setq xref-prompt-for-identifier nil
        xref-search-program 'ripgrep)
  :bind
  ("C-?" . xref-find-references-and-replace))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package avy
    :straight t
    :demand t

    :bind
    ("C-;" . avy-goto-word-1)
    ("<C-m> C-c" . avy-goto-char-2)
    ("<C-m> C-l" . avy-goto-line)
    ("<C-m> C-w" . avy-goto-word-1)
    ("<C-m> <C-m>" . avy-goto-word-1)
    (:map isearch-mode-map
          ("C-;" . avy-isearch))

    :config
    (defun avy-action-teleport-whole-line (pt)
      (avy-action-kill-whole-line pt)
      (save-excursion (yank)) t)

    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)

    (setf (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
          (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package isearch
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        search-whitespace-regexp ".*?"))

(use-package phi-search :straight t)

(setq grep-command "rg -nS --no-heading ")

(use-package p-search :straight (:host github :repo "zkry/p-search"))

(use-package deadgrep :straight t)

(use-package symbol-overlay :straight t)

(provide 'code-navigation-config)
