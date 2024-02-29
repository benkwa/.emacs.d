;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DWA customizations

;; (setq org-agenda-files (list "~/org/inbox.org"
;;                              "~/org/work.org"
;;                              "~/org/now.org"))

; If in a screen, activate normal-erase-is-backspace-mode.
;; (if (getenv "STY")
;;     (normal-erase-is-backspace-mode 0))

(add-to-list 'auto-mode-alist '("SConscript" .  python-mode))
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))


(provide 'bk-dwa)
