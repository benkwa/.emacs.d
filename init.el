(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My packages and customizations

;; Add my customizations to the load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/bk"))

;; scrolling, finding files, switching buffers, browser
(require 'bk-navigation)
(require 'bk-cc)
(require 'bk-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key bindings

(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<C-home>") 'beginning-of-buffer)
(global-set-key (kbd "<C-end>") 'end-of-buffer)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "<M-delete>") 'kill-word)

;; Indent
(global-set-key (kbd "<f4>") 'indent-region)

(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<S-f8>") 'previous-error)

(global-set-key (kbd "<C-tab>") 'switch-to-next-buffer)

;; Fix the buffer when it gets ugly
(global-set-key (kbd "C-c r") 'font-lock-fontify-buffer)

;; This is just annoying
(unbind-key "C-x C-z" global-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colors

;;Enable opposite bracket/paranthesis highlighting
(require 'paren)
(show-paren-mode t)
(setq blink-matching-paren-on-screen t)

;; Maximum decoration for all modes
(setq-default font-lock-maximum-decoration t)

;; Enable font lock (colours) for all modes that support it:
(global-font-lock-mode t)

; Note that `font-lock-use-default-*' must come before `font-lock'.

; We're not to use the default colors
(setq font-lock-use-default-fonts nil)
(setq font-lock-use-default-colors nil)

(require 'font-lock)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab expansion
;; (require 'bk-hippie-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Auto-insert text when making new *.cpp, *.cc, *.h files.
(require 'autoinsert)
;(add-hook 'find-file-hooks 'auto-insert)
(auto-insert-mode)
;; (setq auto-insert-directory "~/emacs/autoinsert/")
;; (define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" ["autoinsert.h" auto-update-header-file])

(setq default-major-mode 'indented-text-mode)
(add-hook 'indented-text-mode-hook 'turn-on-auto-fill)


;; If non-nil each line of text is exactly one screen line, else wrap text.
(setq-default truncate-lines nil)

;; HTML/SGML related stuff

;; DocBook IDE mode
(autoload 'docbook-mode "docbookide" "Major mode for DocBook documents." t)

;; Turn on font lock when in DocBook mode
(add-hook 'docbook-mode-hook
	  'turn-on-font-lock)

(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-to-list 'auto-mode-alist '("\\.gss" . css-mode))
(add-to-list 'auto-mode-alist '("\\.tpl" . html-mode))
(add-to-list 'auto-mode-alist '("crontab" . crontab-mode))
(add-to-list 'auto-mode-alist '("\\.xml" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.html" . html-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.tcsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))

(setq inhibit-default-init t)

;; disable the menu bar
(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc behaviour and other tweaks
(add-to-list 'default-frame-alist '(width . 108))
(add-to-list 'default-frame-alist '(height . 42))

;; We dont't want a startup message
(setq-default inhibit-startup-message t)

(setq confirm-kill-emacs (quote yes-or-no-p))
;; Paste at point NOT at cursor
(setq mouse-yank-at-point 't)

;; Makes things a little bit more consistent.
(fset 'yes-or-no-p 'y-or-n-p)

;Show current line and column in the status field.
(setq line-number-mode t)
(setq column-number-mode t)

;; always use spaces instead of tabs.  Indents are 4 spaces.
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; make the default fill column value 80
(setq-default fill-column 80)

; Show buffer name in title bar
(setq frame-title-format '("emacs: %b - %f"))

;; We don't want to insert newlines when reaching end of buffer
(setq next-line-add-newlines nil)
;; We don't want to be asked about autoinsert
(setq auto-insert-query nil)

;; we want to kill the whole line when doing a Ctrl-k
(setq kill-whole-line t)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d.cache/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d.cache/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d.cache/backups/")))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d.cache/autosaves/" t)

;; Turn on the visible bell, and make it less annoying
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit

(use-package magit
  :defer t

  :custom
  (transient-history-file "~/.emacs.d.cache/transient/history.el")
  (transient-levels-file "~/.emacs.d.cache/transient/levels.el")
  (transient-values-file "~/.emacs.d.cache/transient/values.el")

  :bind
  ("C-x v v" . magit-status)
  )

(use-package git-commit
  :demand t
  :config
  (global-git-commit-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python stuff

(defun bk-python-mode-hook ()
  (local-set-key (kbd "<M-RET>") 'hippie-expand)
  (local-set-key [C-next] 'python-nav-forward-defun)
  (local-set-key [C-prior] 'python-nav-backward-defun)
  (setq indent-tabs-mode nil
        python-indent-line-functions nil
        paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*@[a-z]")
  (local-set-key [C-next] 'python-nav-forward-defun)
  (local-set-key [C-prior] 'python-nav-backward-defun)
; This doesn't appear to work in emacs 23.1
  (python-indent-guess-indent-offset)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (setq fill-column 80)
)

;; append so our custom values win
(add-hook 'python-mode-hook 'bk-python-mode-hook 1)
(add-hook 'python-mode-hook 'subword-mode)
(add-hook 'python-mode-hook 'display-fill-column-indicator-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes stuff

(use-package zenburn-theme
  :demand t
  :config (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic

;(require 'bk-ede)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load DWA customizations
(if (file-exists-p "~/bk/dwa")
    (require 'bk-dwa))

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize stuff
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
