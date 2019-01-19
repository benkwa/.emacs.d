;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DWA customizations

;; Dropbox support for org-mode
(require 'dropbox)
(let ((dropbox-token "~/bk/dropbox-token"))
  (when (file-exists-p dropbox-token)
    (with-temp-buffer
      (insert-file-contents dropbox-token)
      (setq dropbox-access-token (buffer-string)))))

(setq org-agenda-files (list "~/org/inbox.org"
                             "~/org/work.org"
                             "~/org/now.org"))
;; Open links in chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

; If in a screen, activate normal-erase-is-backspace-mode.
(if (getenv "STY")
    (normal-erase-is-backspace-mode 0))

(add-to-list 'auto-mode-alist '("SConscript" .  python-mode))
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))


(provide 'bk-dwa)
