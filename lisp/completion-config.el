;;; completion-config.el --- Description -*- lexical-binding: t; -*-

(use-package consult
   :straight t
   :demand t
   :bind
   ("<C-m> C-i" . consult-imenu)
   ("<C-m> C-s" . consult-line)
   ("M-y" . yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("<C-m> C-d" . consult-mark)
   ("M-g M-m" . consult-mark)
   (:map org-mode-map
         ("<C-m> C-i" . consult-org-heading))
   :custom
   (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000  --smart-case --no-heading --with-filename --line-number"))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(use-package consult-jump-project
  :straight (consult-jump-project :type git :host github :repo "jdtsmith/consult-jump-project")
  :custom
  (consult-jump-direct-jump-modes '(dired-mode))
  (recentf-filename-handlers (lambda (f)
                               (if (file-remote-p f) f
                                 (abbreviate-file-name f))))
  :bind ("C-x p p" . consult-jump-project))

(use-package consult-eglot
  :straight t
  :after eglot)

(use-package embark-consult :straight t)

;; (use-package consult-omni
;;     :straight (consult-omni
;;                :type git
;;                :host github
;;                :repo "armindarvish/consult-omni"
;;                :files (:defaults "sources/*.el"))
;;     :config
;;     (straight-use-package 'request)
;;     (setq consult-omni-show-preview t
;;           consult-omni-preview-key "C-i"
;;           consult-omni-default-count 5
;;           consult-omni-default-input-throttle 1.7
;;           consult-embark-default-term #'vterm
;;           consult-omni-default-browse-function 'browse-url
;;           consult-omni-default-interactive-command #'consult-omni-multi
;;           consult-omni-http-retrieve-backend 'request
;;           consult-omni-open-with-prompt "λ. ")
;;     (require 'consult-omni-sources)
;;     (require 'consult-omni-embark)
;;     (consult-omni-sources-load-modules)
;;     (setq consult-omni-multi-sources '("calc"
;;                                        "File"
;;                                        "Apps"
;;                                        "Google"
;;                                        "GitHub"
;;                                        "Org Agenda")
;;           consult-omni-web-sources '("Wikipedia"
;;                                      "Github")))

;; (defun consult-omni-web (&optional initial prompt sources no-callback &rest args)
;;   "Interactive web search”

;; This is similar to `consult-omni-multi', but runs the search on
;; web sources defined in `consult-omni-web-sources'.  See
;; `consult-omni-multi' for more details.
;; "
;;   (interactive "P")
;;   (let ((prompt (or prompt (concat "[" (propertize "consult-omni-web" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
;;         (sources (or sources consult-omni-web-sources)))
;;     (consult-omni-multi initial prompt sources no-callback args)))

(use-package consult-notmuch
  :straight t
  :after notmuch)

(use-package embark
    :straight t
    :demand t
    :bind
    ("C-x C-j" . embark-act)
    (:map minibuffer-mode-map
          ("C-." . embark-act))
    (:map embark-mode-map
          ("S" . sudo-find-file))
    :config
    (setq prefix-help-command #'embark-prefix-help-command))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package vertico
  :straight t
  :demand t
  :bind (:map vertico-map
            ("C-n" . vertico-next)
            ("C-p" . vertico-previous))
  :config
  (setq vertico-cycle t
        enable-recursive-minibuffers t
        minibuffer-prompt-properties '(read-only t cursor-intangible t
                                       face minibuffer-prompt))
  :init
  (vertico-mode))

(use-package savehist
    :straight t
    :init
    (savehist-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'completion-config)
