;;; init.el --- Description -*- lexical-binding: t; -*-
(use-package hindent
  :straight t
  :after haskell-mode)

(defun hindent-reformat-align-decl ()
"Re-format current declaration using hindent, then align"
  (interactive)
  (let ((start-end (hindent-decl-points)))
    (when start-end
      (let ((beg (car start-end))
            (end (cdr start-end)))
        (hindent-reformat-region beg end t)
        (align beg end)))))

(defun hindent-reformat-align-region (beg end)
"Re-format regionn using hindent, then align"
  (interactive "r")
  (hindent-reformat-region beg end t)
  (align beg end))

(defvar ghc-repo-url "https://gitlab.haskell.org/ghc/ghc/")

(defun open-issue-at-point ()
  (interactive)
  (browse-url (concat ghc-repo-url "-/issues/" (number-to-string (number-at-point)))))

(defun open-MR-at-point ()
  (interactive)
  (browse-url (concat ghc-repo-url "-/merge_requests/" (number-to-string (number-at-point)))))

(use-package dante
  :straight t
  :disabled
  :commands 'dante-mode
  :hook
  (dante-mode . (lambda my-fix-hs-eldoc () (setq eldoc-documentation-strategy #'eldoc-documentation-default)))
  (dante-mode . flymake-mode)
  :init
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :config
  (require 'flymake-flycheck)
  (defalias 'flymake-hlint
    (flymake-flycheck-diagnostic-function-for 'haskell-hlint))
  (add-to-list 'flymake-diagnostic-functions 'flymake-hlint))

(defun insert-haskell-undefined ()
    (interactive)
    (insert "undefined"))

(defun haskell-block-comment-region (start end)
  (interactive "r")
  (save-excursion
    (let (end-marker)
      (goto-char end)
      (setq end-marker (point-marker))
      (goto-char start)
      (insert "{-\n")
      (goto-char (marker-position end-marker))
      (insert "-}"))))

;; (defun haskell-comment-equations (start end)
;;   (interactive "r")
;;   (save-excursion
;;     (let ((name (sexp-at-point)))
;;       (isearch-forward name)
;;       (beginning-of-visual-line))))

(defun haskell-mode-copy-module-name (buffer)
  "Puts the module name into the killring"
  (message "TODO"))

(with-eval-after-load 'align
    (add-to-list 'align-rules-list
                      '(haskell-types
                        (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-assignment
                        (regexp . "\\(\\s-+\\)=\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-arrows
                        (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-left-arrows
                        (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-types
                        (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-assignment
                        (regexp . "\\(\\s-+\\)=\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-arrows
                        (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode))))
         (add-to-list 'align-rules-list
                      '(haskell-left-arrows
                        (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                        (modes quote (haskell-mode haskell-literate-mode)))))

(with-eval-after-load 'w3m
  (add-hook 'w3m-display-hook 'w3m-haddock-display))

(use-package haskell-snippets
  :straight t)

(use-package hiedb-mode
  :straight
  (hiedb-mode
   :type git
   :host github
   :repo "agentultra/hiedb-mode"
   :files (".el")))

(use-package consult-hoogle
  :straight t
  :after haskell-mode
  :config
  (eval-when-compile
    (defvar vertico-multiform-commands))
  (add-to-list 'vertico-multiform-commands
               '(consult-hoogle buffer)))

(defun fix-doom-1337-haskell-faces ()
  "Change the default faces for `haskell-constructor-face' and
`haskell-type-face'set by doom-1337 to orange"
  (interactive)
  (let ((doom-1337-theme (custom-theme-enabled-p 'doom-1337))
        (CL-face (face-attribute 'font-lock-type-face :foreground))
        (orange "#ff9200")
        (faces (list 'haskell-type-face 'haskell-constructor-face))
        (correct-haskell-type-face ))
    (mapcar (lambda (face)
              (set-face-foreground face (if doom-1337-theme
                                            orange
                                          CL-face)))
            faces)))

(defun artin/foo (beg end)
  (interactive "r")
  (let* ((pat-str (buffer-substring beg end))
         (binding-var-regexp "\s[^,{]=?\s[[:lower:]]+\\([[:digit:]]+\\)+")
         (matched-str (string-match binding-var-regexp pat-str))
         (num-suffix-str (match-string 1))
         (new-num-suffix (pp-to-string (1+ (string-to-number num-suffix-str)))))
    (save-excursion
      (end-of-buffer)
      (insert matched-str))))

(defun haskell-copy-pat-inc-fvs (beg end)
  "Copy pattern to the kill ring and increcment the number
at the end of its variable
Example: (Just v1) becomes (Just v2)
         (v1, v2) becomes (v3, v4)"
  (interactive "r")
  (let* ((pat-str (buffer-substring beg end))
         (binding-var-regexp "\s[^,{]=?\s[[:lower:]]+\\([[:digit:]]+\\)+")
         (matched-str (string-match binding-var-regexp pat-str))
         (num-suffix-str (match-string 1))
         (new-num-suffix (pp-to-string (1+ (string-to-number num-suffix-str)))))
    (save-excursion
      (end-of-buffer)
      (insert (replace-match new-num-suffix nil nil matched-str 1)))))

(use-package haskell-mode
  :straight t
  :demand t
  :config
  (require 'subword)
  (setq haskell-font-lock-symbols nil
        haskell-stylish-on-save nil
        haskell-process-log t
        haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")
        haskell-process-sugggest-hoogle-imports t)
  :bind
  (:map haskell-mode-map
        ("<C-m> C-w" . avy-goto-subword-1)
        ("<C-m> <C-m>" . avy-goto-subword-1)
        ("C-;" . avy-goto-subword-1)
        ("C-c C-;" . haskell-block-comment-region)
        ("C-c M-o" . haskell-mode-tag-find)
        ("C-c h" . consult-hoogle)
        ("C-c C-o C-o" . open-GHC-style-note-at-point)
        ("C-c C-o n" . open-GHC-style-note)
        ("C-c C-o i" . open-issue-at-point)
        ("C-c C-o m" . open-MR-at-point)
        ("C-c i p" . haskell-command-insert-language-pragma)
        ("C-c C-u" . insert-haskell-undefined)
        ("C-c i s" . haskell-mode-toggle-scc-at-point)
        ("C-c C-d" . haskell-process-do-info)
        ("C-c i m" . haskell-add-import)
        ("C-c m" . haskell-navigate-imports)
        ("C-c C-n" . haskell-ds-forward-decl)
        ("C-c C-p" . haskell-ds-backward-decl)
        ("C-x C-n" . next-error)
        ("C-x C-p" . previous-error)
        ("M-n" . haskell-ds-forward-decl)
        ("M-p" . haskell-ds-backward-decl)
        ("M-g M-w" . avy-goto-subword-1)
        ("C-S-f" . subword-forward)
        ("C-S-b" . subword-backward)
        ("C-S-d" . subword-kill)
        ("C-S-t" . subword-transpose)
        ("C-<backspace>" . subword-backward-kill)
        ("M-S-<backspace>" . subword-backward-kill))
  :hook
  (haskell-mode . haskell-auto-insert-module-template)
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . hindent-mode))

;; TODO: Make this ask for options to pass to cabal repl so that we can
;; have support for multiple home units
;; (defun haskell-process-load-file-ask ()
;;   "Load the current buffer file."
;;   (interactive)
;;   (save-buffer)
;;   (haskell-interactive-mode-reset-error (haskell-session))
;;   (haskell-process-file-loadish (format "load \"%s\"" (replace-regexp-in-string
;;                                                        "\""
;;                                                        "\\\\\""
;;                                                        (buffer-file-name)))
;;                                 nil
;;                                 (current-buffer)))

(use-package haskell-ng-mode
  :straight (:type git
             :repo "https://gitlab.com/magus/haskell-ng-mode"
             :branch "main")
  :disabled
  :init
  (add-to-list 'treesit-language-source-alist '(haskell "https://github.com/tree-sitter/tree-sitter-haskell"))
  (add-to-list 'treesit-language-source-alist '(cabal "https://gitlab.com/magus/tree-sitter-cabal.git"))
  (treesit-install-language-grammar 'haskell)
  (treesit-install-language-grammar 'cabal))

(provide 'haskell-config)
