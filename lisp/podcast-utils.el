(setq podcast-utils/default-description "Background noise")

(defun podcast-utils/episode-note-buffer ()
  (file-name-sans-extension (file-name-nondirectory
                             (emms-track-name
                              (emms-playlist-current-selected-track)))))

(defun podcast-utils/write-to-episode-note-buffer (&optional description)
  (interactive)
  (cl-flet ((write-timerange (buffer &optional description)
              (let* ((timestamp emms-playing-time)
                     (timestamp-range (concat (emms-playing-time-format-time (- (truncate timestamp) 1))
                                              " -- "
                                              (emms-playing-time-format-time (+ (truncate timestamp) 1)))))

                (save-excursion (with-current-buffer (get-buffer-create buffer)
                                  (if description
                                      (insert (concat timestamp-range " : " description "\n"))
                                    (insert (concat timestamp-range
                                                    " : "
                                                    podcast-utils/default-description "\n"))))))))
    (write-timerange (podcast-utils/episode-note-buffer) description)))

(defun podcast-utils/open-write-note (note)
  (interactive
   (progn
     (emms-pause)
     (let
         ((note (read-string "Note: " nil)))
       (list note))))
  (podcast-utils/write-to-episode-note-buffer note)
  (emms-pause))

(defun podcast-utils/open-episode-buffer ()
  (interactive)
  (switch-to-buffer (podcast-utils/episode-note-buffer)))

(provide 'podcast-utils)
