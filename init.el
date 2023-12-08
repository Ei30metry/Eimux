;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Artin Ghasivand
;;
;; Author: Artin Ghasivand <ghasivand.artin@gmail.com>
;; Maintainer: Artin Ghasivand <ghasivand.artin@gmail.com>
;; Created: August 05, 2022
;; Modified: August 05, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/artin/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(org-babel-load-file
    (expand-file-name
      "config.org"
      user-emacs-directory))
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "0ec1d50ee7c886bd065aacff1a6a5034a32357c89a07561fd14f64dfcbf0cf6d" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "02fefdfc9a0c7256a10c8794a4985c9c70c5fbf674873b66807e8143e02c81a7" "48e64636190d872847debc9665eada87e29c1bc4405d4859ef8161e4f2313120" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "85dfc58d150f35da8c788e04b21e282e45dc09c8ace7ff669c3c7b5a35f95afc" "8d34de60a929c2a9402a574a013c435b0901d81455541d0d561fad831d1d5715" default))
 '(haskell-process-log t)
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   '(helpful exec-path-from-shell toc-org company-ghci company-cabal evil-collection evil use-package))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package) (use-package) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
