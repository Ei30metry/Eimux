(setq frame-resize-pixelwise t)
(setq package-enable-at-startup nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
;; (setq inhibit-startup-message 't)
;; (setq initial-scratch-message 'nil)
;; (setq initial-major-mode 'fundamental-mode)

;; (setq jit-lock-stealth-time nil)
;; (setq jit-lock-defer-time nil)
;; (setq jit-lock-defer-time 0.0)
;; (setq jit-lock-stealth-load 200)

(provide 'early-init)
