(defun artin/remove-project ()
  "Remove a project from the project list file"
  (interactive)
  (let ((prj (completing-read "Project to remove: " project--list)))
    (project--remove-from-project-list prj
                                       (format "Removed %s from the project list."
                                               prj))))

(setq project-vc-extra-root-markers '(".project"
                                      "*.cabal"
                                      "requirements.txt"
                                      "autogen.sh"))

(global-set-key (kbd "C-x p /") 'consult-ripgrep)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)
(global-set-key (kbd "C-x p C") 'project-recompile)

(provide 'project-config)
