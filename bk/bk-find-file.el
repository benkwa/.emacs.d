;;; bk-find-file.el

;;; using ido find file in tag files
(defun tags-extra-get-all-tags-files ()
  "Return all, fully qualified, file names."
  (save-excursion
    (let ((first-time t)
          (res nil))
      (while (visit-tags-table-buffer (not first-time))
        (setq first-time nil)
        (setq res
              (append res (mapcar 'expand-file-name (tags-table-files)))))
      res)))

(defun ido-find-file-in-tag-files ()
  (interactive)
  (find-file
   (expand-file-name
    (ido-completing-read
     "Files: " (tags-extra-get-all-tags-files) nil t))))

(defun bk-find-file(arg)
  "Runs find-file (with prefix arg) or find-file-in-tags (without)"
  (interactive "P")
  (if (or (not tags-file-name) arg)
      ; if called with a prefix arg, or if there is no tags-file, just run
      ; find-file.
      (call-interactively 'find-file)
    ; else, call find-file-in-tags
    (call-interactively 'ido-find-file-in-tag-files)))

(provide 'bk-find-file)
