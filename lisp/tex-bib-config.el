;;; tex-bib-config.el --- Description -*- lexical-binding: t; -*-
(use-package cdlatex
    :straight t)

(use-package auctex
    :straight t
    :demand t
    :hook
    (LaTeX-mode . reftex-mode)
    (LaTeX-mode . cdlatex-mode)
    (LaTeX-mode . jinx-mode)
    (LaTeX-mode . display-line-numbers-mode)
    (LaTeX-mode . prettify-symbols-mode)
    (LaTeX-mode . smartparens-mode)
    (tex-after-compilation-finished-functions . TeX-revert-document-buffer)
    :bind
    ;; (:map LaTeX-mdoe-map
    ;; ("<C-m> C-w" . avy-goto-subword-1)
    ;; ("<C-m> <C-m>" . avy-goto-subword-1)
    ;; ("C-S-f" . subword-forward)
    ;; ("C-S-b" . subword-backward)
    ;; ("C-S-k" . subword-kill)
    ;; ("C-S-t" . subword-transpose)
    ;; ("C-<backspace>" . subword-backward-kill))
    :mode
    ("\\.tex\\'" . LaTeX-mode)
    ("\\.mng\\'" . LaTeX-mode)
    ("\\.lhs\\'" . LaTeX-mode))

(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/Research/artin.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode   . citar-capf-setup))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(use-package biblio
  :straight t
  :config
  (require 'biblio-download))

(provide 'tex-bib-config)
