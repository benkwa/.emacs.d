;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js stuff

;; (setq-default comment-style 'extra-line)

;; js error finding in compilation buffers
;(pushnew '(bk-js "^##[[:space:]]*\\([a-zA-Z0-9_/]+\.js\\):\\([0-9]+\\):[[:space:]]*\\(WARNING\\|ERROR\\)*" 1 2)
;         compilation-error-regexp-alist-alist)
;; only support js errors for now
;; (setq compilation-error-regexp-alist (list 'bk-js))

(defun bk-js-mode-hook ()
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  (local-set-key (kbd "<tab>") 'bk-tab-dwim)
  (local-set-key (kbd "TAB") 'bk-tab-dwim)

  (setq indent-line-function 'js2-indent-line)
  (setq js2-basic-offset 2)

  (setq skeleton-pair 1)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
)

(add-hook 'js-mode-hook 'bk-js-mode-hook)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'electric-indent-mode)
(add-hook 'js-mode-hook 'electric-pair-mode)


;; highlight stuff that goes over the 80-char limit
(make-face 'my-80col-face)
(set-face-attribute 'my-80col-face nil :foreground "yellow":background "red")
(add-hook 'js-mode-hook (lambda()
                          (font-lock-add-keywords
                           nil
                           '(("^.\\{80\\}\\(.\\)" 1 'my-80col-face prepend)))))
