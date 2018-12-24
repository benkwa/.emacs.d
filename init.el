;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Add packages and its subdirs to the load path.
(let ((default-directory "~/.emacs.d/packages"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My packages and customizations

;; Add my customizations to the load path.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; org mode customizations
(require 'bk-org)

;; semantic and ede customizations
;; experimental; not working yet
; (require 'bk-ede)

;; snippets and expansions
(require 'bk-yasnippets)

(require 'dropbox)
(if (require 'bk-dropbox-token nil t)
    (setq dropbox-access-token bk-dropbox-access-token))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key bindings

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<C-home>") 'beginning-of-buffer)
(global-set-key (kbd "<C-end>") 'end-of-buffer)
(global-set-key (kbd "C-z") 'undo)


;; Indent
(global-set-key (kbd "<f4>") 'indent-region)

(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<S-f8>") 'previous-error)

(global-set-key (kbd "<C-tab>") 'switch-to-next-buffer)

;; Fix the buffer when it gets ugly
(global-set-key (kbd "C-c r") 'font-lock-fontify-buffer)

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
;; scrolling, navigation, tag-related stuff
(require 'bk-navigation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab expansion
(require 'bk-hippie-expand)

(defun bk-tab-dwim ()
  "If in the minibuffer, call minibuffer-expand. Else, if mark is
    active, indent region. Else if point is at the end of a
    symbol, expand it. Else indent the current line."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (let ((p (point)))
        (indent-according-to-mode)
        (when (and (= p (point))
                   (not (bolp)))
          (hippie-expand nil))))))
                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hungry delete ALL the things.
(require 'hungry-delete)
(global-hungry-delete-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Auto-insert text when making new *.cpp, *.cc, *.h files.
(require 'autoinsert)
;(add-hook 'find-file-hooks 'auto-insert)
(auto-insert-mode)
;; (setq auto-insert-directory "~/emacs/autoinsert/")
;; (define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" ["autoinsert.h" auto-update-header-file])

(setq default-major-mode 'indented-text-mode)
(add-hook 'indented-text-mode-hook 'turn-on-auto-fill)

;; texinfo
(add-hook 'Info-mode-hook
  '(lambda() (define-key Info-mode-map [down-mouse-1] 
	       'Info-mouse-follow-nearest-node)))
;; Buffer Menu
(add-hook 'buffer-menu-mode-hook
  '(lambda() (define-key Buffer-menu-mode-map [down-mouse-1] 
	       'Buffer-menu-mouse-select)))

;; If non-nil each line of text is exactly one screen line, else wrap text.
(setq-default truncate-lines nil)

(setq imenu-always-use-completion-buffer-p t)


(defun change-var-in-file( var file val )
  "Changes the variable named var in the given file with the given val and saves it"
  (let (buf)
    (save-excursion
      (setq buf (find-file-noselect file))
      (set-buffer buf)
      (beginning-of-buffer)
      (if (search-forward-regexp (concat "^(defvar[ \t]+"
					 var
					 "[ \t]+\\(t\\|nil\\))")
				 nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 1) (match-end 1))
	    (replace-match val t nil nil 1)
	    (save-buffer))))))


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
;; build stuff

(require 'compile)

;; TODO unhackify this
(setq-default compile-command "git build")
(global-set-key (kbd "<f7>") 'compile)

;; follow output in the compile window
(setq compilation-scroll-output 't)

;; don't ask for the compile command, save all files
(setq compilation-read-command nil)
(setq compilation-ask-about-save nil)

;; make compilers in a new frame.
;; (add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))

;; make compilation show up in a separate frame
;; (setq special-display-buffer-names
;;       '(("*compilation*" . ((name . "*compilation*")
;;                             ,@default-frame-alist
;;                             (left . ( - 1))
;;                             (top . 0)
;;                             (width . 180)
;;                             (height . 35)))))

(setq compile-command "bkbuild")



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

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;(require 'magit)
;; We don't use vc-next-action anyways; just use existing muscle memory.
(global-set-key (kbd "C-x v v") 'magit-status)
;; Disable annoying magit warnings
(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight long lines

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
that uses 'compilation-error-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 compilation-error-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 120))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'js-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python stuff

(defun bk-python-mode-hook ()
  (local-set-key (kbd "<M-RET>") 'hippie-expand)
  (setq indent-tabs-mode nil
        python-indent-line-functions nil
        paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*@[a-z]")
  (local-set-key [C-next] 'python-nav-forward-defun)
  (local-set-key [C-prior] 'python-nav-backward-defun)
; This doesn't appear to work in emacs 23.1
  (python-indent-guess-indent-offset)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
)

;; append so our custom values win 
(add-hook 'python-mode-hook 'bk-python-mode-hook 1)
(add-hook 'python-mode-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java stuff

(defun bk-java-mode-hook () 
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  (local-set-key (kbd "<tab>") 'bk-tab-dwim)
  (local-set-key (kbd "TAB") 'bk-tab-dwim)

  (setq skeleton-pair 1)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  (set-fill-column 100)
)
(add-hook 'java-mode-hook 'bk-java-mode-hook)
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'java-mode-hook 'electric-indent-mode)
(add-hook 'java-mode-hook 'electric-pair-mode)

;; Android customizations
; append rather than prepend this because it overrides other java mode settings
(defun bk-android-java-hook () 
  (setq c-basic-offset 4)
)
;(add-hook 'java-mode-hook 'bk-android-java-hook 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpp stuff

(defun bk-arglist-cont (elem)
  ;; (save-excursion
  ;;   (let ((indent-pos)))
  ;;   )
  (save-excursion
    (back-to-indentation)
    (let ((col (current-column))
          (cur (+ (c-langelem-col c-syntactic-element) (* 2 c-basic-offset))))
      (message "current column: %d" col)
      (message "langelem-col: %d" cur)
      (if (not (= cur col))
        (progn
          (message "returning 0")
          (vector cur))))))

(defun bk-c++-mode-hook ()
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  ;; (local-set-key (kbd "<tab>") 'bk-tab-dwim)
  ;; (local-set-key (kbd "TAB") 'bk-tab-dwim)
;  (local-set-key (kbd "<C-return>") 'semantic-complete-analyze-and-replace)
   (local-set-key (kbd "<C-return>") 'hippie-expand)
  (local-set-key (kbd "C-c C-x o") 'ff-find-other-file)
 (setq skeleton-pair 1)
;  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  (setq c-offsets-alist
        (append
         (list
          '(innamespace . 0)
          '(arglist-intro . ++)
          '(arglist-cont-nonempty bk-arglist-cont c-lineup-arglist)
          )
         c-offsets-alist))
  (set-fill-column 120)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)
(add-hook 'c++-mode-hook 'bk-c++-mode-hook)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'electric-indent-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(setq electric-pair-pairs '(
                            (?{ . ?})
                            (?[ . ?])
                            (?( . ?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(require 'smerge-mode)
(defface smerge-mine-bk
  '((((min-colors 88) (background light))
     (:foreground "blue1"))
    (((background light))
     (:foreground "blue"))
    (((min-colors 88) (background dark))
     (:foreground "cyan1"))
    (((background dark))
     (:foreground "cyan")))
  "Face for your code."
  :group 'smerge)
(defface smerge-other
  '((((background light))
     (:foreground "magenta"))
    (((background dark))
     (:background "magenta")))
  "Face for the other code."
  :group 'smerge)
(defface smerge-refined-added
  '((t :background "dark olive green"))
    "foobar"
    :group 'smerge)
;;(setq smerge-other-face 'smerge-other-bk)

(defun bk-kill-emacs()
  (save-some-buffers 1)
  (kill-emacs))

;(add-to-list 'default-frame-alist '(font . "lucidasanstypewriter-14"))
;; new metacity has some font bug that causes emacs to hang up waiting
;; for a response when setting the font.  Tell emacs not to wait for
;; the wm.
;;(modify-frame-parameters nil '((wait-for-wm . nil)))

;; (defun my-display-completions (buf)
;;   "put the *completions* buffer in a small, new window below the current one"
;;   (if (active-minibuffer-window)
;;       ; minibuffer is active - show completion buffer the regular way
;;       (let (special-display-buffer-names)
;; 	(display-buffer buf))
;;       ; else, we're in a regular editing window - split it and show
;;       ; the completion buffer below
;;       (let ((target-window (split-window-vertically -10)))
;; 	(set-window-buffer target-window buf)
;; 	target-window)
;;   )
;; )

;; display line numbers
;; (require 'wb-line-number)
;; (setq wb-line-number-scroll-bar nil)
;; (setq wb-line-number-text-width 4)
;; (require 'setnu)

;; find files in tags
;; (require 'find-file-in-tags)

;; (autoload 'gtags-mode "gtags" "" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes stuff

(if (version< emacs-version "24")
    (progn
      (require 'color-theme)
      (require 'zenburn)
      (color-theme-zenburn))
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/packages/zenburn-emacs")
    (load-theme 'zenburn t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic

;(require 'bk-ede)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load DWA customizations
(if (file-exists-p "~/bk/dwa")
    (require 'bk-dwa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (request org-edna magithub f magit yasnippet js2-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:foreground "green2" :underline t))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#308430")))))
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el
