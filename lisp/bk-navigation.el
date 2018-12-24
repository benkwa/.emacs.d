;; scrolling and navigation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling

;; Set scroll-margin to 0 for smooth scrolling.
(setq scroll-margin 0)
(setq hscroll-step 1)

;; Scroll one vertical line, 0 means center.
;; (setq scroll-step 0)
;; A value >100 means that scrolling point of-screen will never re-center it.
(setq scroll-conservatively 101)

;; no scroll bars
(set-scroll-bar-mode nil)

(defun bk-scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(defun bk-scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "<C-up>") 'bk-scroll-down-one-line)
(global-set-key (kbd "M-p") 'bk-scroll-down-one-line)
(global-set-key (kbd "<C-down>") 'bk-scroll-up-one-line)
(global-set-key (kbd "M-n") 'bk-scroll-up-one-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer management

;; Jumps to next buffer in the bufferlist, consecutive uses will browse trough all your buffers
(defun switch-to-next-buffer()
  "Jumps to next buffer in the buffer list, or the beginning of the list if at the end"
  (interactive)
  (let ((cur (current-buffer))
	(ok t))
    ;; Find current buffer in list
    (while (and cur ok)
      (setq cur (next-buffer cur (current-buffer)))
      (if cur
	  (if (buffer-allowed cur)
	      (setq ok nil))))
    (if (and cur (not ok))
	(switch-to-buffer cur t))))

;; Jumps to next buffer in the bufferlist, consecutive uses will browse trough all your buffers
(defun next-buffer(buf orig)
  "Jumps to next buffer in the buffer list, or the beginning of the list if at the end"
  (interactive)
  (let ((lst (buffer-list))
	nxt
	cur)
    ;; Find current buffer in list
    (while (and lst (not (eq buf (car lst))))
      (setq cur (car lst))
      (setq lst (cdr lst)))
    ;; Get next
    (setq nxt (car (cdr lst)))
    (if (eq nxt orig)
	nil)
    ;; If zero get first.
    (if nxt
	()
      (setq nxt (car (buffer-list))))
    nxt))

(defun buffer-allowed( buf )
  (interactive)
  (let ((incs buffer-include-regexp)
	inc
	(bname (buffer-name buf))
	(allow nil))
    (while (and incs (not allow))
      (setq inc (car incs))
      (if (string-match inc bname)
	  (setq allow t))
      (setq incs (cdr incs)))
    (if allow
	(let ((exs buffer-exclude-regexp)
	      ex)
	  (while (and exs allow)
	    (setq ex (car exs))
	    (if (string-match ex bname)
		(setq allow nil))
	    (setq exs (cdr exs)))
	  allow)
      allow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag support

;(require 'find-file-in-tags)
;(require 'etags-select)

(defun bk-find-file(arg)
  "Runs find-file (with prefix arg) or find-file-in-tags (without)"
  (interactive "P")
  (if (or (not tags-file-name) arg)
      ; if called with a prefix arg, or if there is no tags-file, just run
      ; find-file.
      (call-interactively 'find-file)
    ; else, call find-file-in-tags
    (call-interactively 'find-file-in-tags)))
(global-set-key (kbd "C-x C-f") 'bk-find-file)

;; read in the local tags file if one exists
(let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

(global-set-key (kbd "M-.") 'etags-select-find-tag)
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)


(provide 'bk-navigation)
;;; bk-navigation.el ends here
