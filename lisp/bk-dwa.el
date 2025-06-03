;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DWA customizations

;; Open links in chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Various backspace-fixing attempts

; If in a screen, activate normal-erase-is-backspace-mode.
;; (if (getenv "STY")
;;     (normal-erase-is-backspace-mode 0))

;; (require 'bk-frame-hooks)
;; (defun bk-fix-backspace ()
;;   (interactive)
;;   (message "Fixing backspace!!!")
;;   (normal-erase-is-backspace-setup-frame)
;;   (when (getenv "STY") ; check if in screen
;;     (normal-erase-is-backspace-mode 1)
;;     (message "Fixed backaspace")))
;; (add-hook 'after-make-console-frame-hooks 'bk-fix-backspace)

(add-to-list 'auto-mode-alist '("SConscript" .  python-mode))
;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; Example build command (bamboo)
;; rez-env buildtools-1.1 -c 'rez-build -i --variants 1 -p'${bamboo.build.working.directory}/install' -- --print-test-results -k @run_all --unittest-xml --disable-gdb-stacktraces --taskmastertrace=/tmp/taskmastertrace.txt BAMBOO_BUILDKEY=${bamboo.buildKey} BAMBOO_BUILDNUM=${bamboo.buildNumber}'
;;(add-to-list 'auto-mode-alist '("build.log" . build-log-mode))

;; Colorize build logs
(defun bk-colorize-logs ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Build logs examples
;; https://writequit.org/articles/working-with-logs-in-emacs.html

(provide 'bk-dwa)
