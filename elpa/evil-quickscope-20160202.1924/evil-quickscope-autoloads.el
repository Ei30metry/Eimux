;;; evil-quickscope-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-quickscope" "evil-quickscope.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-quickscope.el

(autoload 'evil-quickscope-find-char "evil-quickscope" "\
Move to the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word." t nil)

(autoload 'evil-quickscope-find-char-backward "evil-quickscope" "\
Move to the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word." t nil)

(autoload 'evil-quickscope-find-char-to "evil-quickscope" "\
Move before the next COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word." t nil)

(autoload 'evil-quickscope-find-char-to-backward "evil-quickscope" "\
Move before the previous COUNT'th occurence of CHAR.
Highlight first or second unique letter of each word." t nil)

(autoload 'evil-quickscope-always-mode "evil-quickscope" "\
Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highglights always on.

This is a minor mode.  If called interactively, toggle the
`Evil-Quickscope-Always mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-quickscope-always-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-evil-quickscope-always-mode 'globalized-minor-mode t)

(defvar global-evil-quickscope-always-mode nil "\
Non-nil if Global Evil-Quickscope-Always mode is enabled.
See the `global-evil-quickscope-always-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-quickscope-always-mode'.")

(custom-autoload 'global-evil-quickscope-always-mode "evil-quickscope" nil)

(autoload 'global-evil-quickscope-always-mode "evil-quickscope" "\
Toggle Evil-Quickscope-Always mode in all buffers.
With prefix ARG, enable Global Evil-Quickscope-Always mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Quickscope-Always mode is enabled in all buffers where
`turn-on-evil-quickscope-always-mode' would do it.

See `evil-quickscope-always-mode' for more information on
Evil-Quickscope-Always mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-quickscope-always-mode "evil-quickscope" "\
Enable `evil-quickscope-mode'." t nil)

(autoload 'turn-off-evil-quickscope-always-mode "evil-quickscope" nil t nil)

(autoload 'evil-quickscope-mode "evil-quickscope" "\
Quickscope mode for evil. Highlights per-word targets for f,F,t,T vim
movement commands. Target highlights activate when f,F,t,T pressed.

This is a minor mode.  If called interactively, toggle the
`Evil-Quickscope mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-quickscope-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-evil-quickscope-mode 'globalized-minor-mode t)

(defvar global-evil-quickscope-mode nil "\
Non-nil if Global Evil-Quickscope mode is enabled.
See the `global-evil-quickscope-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-quickscope-mode'.")

(custom-autoload 'global-evil-quickscope-mode "evil-quickscope" nil)

(autoload 'global-evil-quickscope-mode "evil-quickscope" "\
Toggle Evil-Quickscope mode in all buffers.
With prefix ARG, enable Global Evil-Quickscope mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Quickscope mode is enabled in all buffers where
`turn-on-evil-quickscope-mode' would do it.

See `evil-quickscope-mode' for more information on Evil-Quickscope
mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-quickscope-mode "evil-quickscope" "\
Enable `evil-quickscope-mode'." t nil)

(autoload 'turn-off-evil-quickscope-mode "evil-quickscope" "\
Disable `evil-quickscope-mode'." t nil)

(register-definition-prefixes "evil-quickscope" '("evil-quickscope-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-quickscope-autoloads.el ends here
