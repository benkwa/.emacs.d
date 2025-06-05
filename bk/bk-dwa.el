;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DWA customizations

;; Open links in chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(setq bk/browser-function 'browse-url-chrome)

;; Various backspace-fixing attempts

; If in a screen, activate normal-erase-is-backspace-mode.
;; (if (getenv "STY")
;;     (normal-erase-is-backspace-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame-hooks for fixing backspace key behaviour

;; (defvar after-make-console-frame-hooks '()
;;   "Hooks to run after creating a new TTY frame")
;; (defvar after-make-window-system-frame-hooks '()
;;   "Hooks to run after creating a new window-system frame")

;; (defun run-after-make-frame-hooks (frame)
;;   "Run configured hooks in response to the newly-created FRAME.
;; Selectively runs either `after-make-console-frame-hooks' or
;; `after-make-window-system-frame-hooks'"
;;   (with-selected-frame frame
;;     (run-hooks (if window-system
;;                    'after-make-window-system-frame-hooks
;;                  'after-make-console-frame-hooks))))

;; (add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

;; (defconst sanityinc/initial-frame (selected-frame)
;;   "The frame (if any) active during Emacs initialization.")

;; (add-hook 'after-init-hook
;;           (lambda () (when sanityinc/initial-frame
;;                   (run-after-make-frame-hooks sanityinc/initial-frame))))

;; (defun bk-test-window-frame ()
;;   (message "Window frame hook"))
;; (defun bk-test-console-frame ()
;;   (message "Console frame hook"))

;; (add-hook 'after-make-window-system-frame-hooks 'bk-test-window-frame)
;; (add-hook 'after-make-console-frame-hooks 'bk-test-console-frame)

;; (defun bk-fix-backspace ()
;;   (interactive)
;;   (message "Fixing backspace!!!")
;;   (normal-erase-is-backspace-setup-frame)
;;   (when (getenv "STY") ; check if in screen
;;     (normal-erase-is-backspace-mode 1)
;;     (message "Fixed backaspace")))
;; (add-hook 'after-make-console-frame-hooks 'bk-fix-backspace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
