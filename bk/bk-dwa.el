;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DWA customizations


(message "enabling elpy")
(elpy-enable)
(setq elpy-rpc-virtualenv-path "~/.emacs.d.cache/elpy")
(message "elpy enabled")

(add-to-list 'auto-mode-alist '("SConscript" .  python-mode))
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; Build logs examples
;; https://writequit.org/articles/working-with-logs-in-emacs.html
;;(add-to-list 'auto-mode-alist '("build.log" . build-log-mode))

;; Colorize build logs
(defun bk-colorize-logs ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'bk-dwa)
