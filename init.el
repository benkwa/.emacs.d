;; add .site-lisp and site-lisp in home directory to load path
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

; Add elisp and subdirs to the load path.
;; (let ((default-directory "~/.emacs.d/elisp"))
;;   (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(let ((default-directory "~/.emacs.d/packages"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives
            '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;(with-eval-after-load "git-commit-mode" (debug))
;(defun git-commit-mode (&rest args) nil)

; If in a screen, activate normal-erase-is-backspace-mode.
(if (getenv "STY")
    (normal-erase-is-backspace-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling and navigation

;; Set scroll-margin to 0 for smooth scrolling.
(setq scroll-margin 0)
(setq hscroll-step 1)

;; Scroll one vertical line, 0 means center.
;; (setq scroll-step 0)
;; A value >100 means that scrolling point of-screen will never re-center it.
(setq scroll-conservatively 101)

;; no scroll bars
(set-scroll-bar-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key bindings

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "<M-right>") 'forward-word)
(global-set-key (kbd "<M-left>") 'backward-word)
(global-set-key (kbd "<C-home>") 'beginning-of-buffer)
(global-set-key (kbd "<C-end>") 'end-of-buffer)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "<C-up>") 'bk-scroll-down-one-line)
(global-set-key (kbd "M-p") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<C-down>") 'bk-scroll-up-one-line)
(global-set-key (kbd "M-n") '(lambda () (interactive) (scroll-up 1)))


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

; Transparent windows in term mode
;; (add-hook 'after-make-frame-functions
;;           (lambda ()
;;             (if (not display-graphic-p)
;;                 (set-background-color "ARGBBB000000"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag-related stuff
(require 'bk-find-file)

(global-set-key (kbd "C-x C-f") 'bk-find-file)

;; read in the local tags file if one exists
(let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
  (when my-tags-file
    (message "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

;; (if (file-exists-p "./TAGS")
;;      (visit-tags-table "./TAGS")
;; )

;(global-set-key (kbd "M-.") 'etags-select-find-tag)
;(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;(require 'bk-yasnippets)

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


;; (defun auto-update-header-file()
;;   (save-excursion
;;     (correct-c-header-define (current-buffer)))
;;     (if (search-forward-regexp "[0-9][0-9][0-9][0-9]" nil t)
;;         (replace-match "1234")))

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

;; You might want to make this the default for .sgml or .xml documents,
;; or you might want to rely on -*- DocBook -*- on the first line,
;; or perhaps buffer variables. It's up to you...
(setq auto-mode-alist
      (append
       (list
	'("\\.css" . css-mode)
	'("\\.gss" . css-mode)
	'("\\.tpl" . html-mode)
	'("crontab" . crontab-mode)
	'("\\.xml" . xml-mode)
	'("\\.html" . html-mode)
        '("\\.h\\'" . c++-mode)
        '("\\.inl\\'" . c++-mode)
        '("\\.cu\\'" . c++-mode)
        '("\\.mojom\\'" . c++-mode)
        '("\\.sh\\'" . shell-script-mode)
        '("\\.tcsh\\'" . shell-script-mode)
        '("\\.bash\\'" . shell-script-mode)
        '("\\.env\\'" . shell-script-mode)
        '("\\.ebuild\\'" . shell-script-mode))
       auto-mode-alist))

;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
;; (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

(setq inhibit-default-init t)

;; disable the menu bar
(menu-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build stuff

(require 'compile)

;; TODO unhackify this
(setq-default compile-command "git build") ;; //photos/chromeapp:pulsar_unpacked_debug
(global-set-key (kbd "<f7>") 'compile)

;; follow output in the compile window
(setq compilation-scroll-output 't)

;; don't ask for the compile command, save all files
(setq compilation-read-command nil)
(setq compilation-ask-about-save nil)
;(setq compile-command "/path/to/script")
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

;; camelCamelCamel
(subword-mode)

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
;; building stuff
(setq compile-command "bkbuild")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;; (require 'magit)
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
(font-lock-add-keywords 'borg-mode (font-lock-width-keyword 80))

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
;; js stuff

;; (setq-default comment-style 'extra-line)

;; js error finding in compilation buffers
;; (pushnew '(bk-js "^##[[:space:]]*\\([a-zA-Z0-9_/]+\.js\\):\\([0-9]+\\):[[:space:]]*\\(WARNING\\|ERROR\\)*" 1 2)
;;          compilation-error-regexp-alist-alist)
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

;; soy
(autoload 'closure-template-html-mode "closure-template-html-mode" "Major mode for soy templates." t)
(add-to-list 'auto-mode-alist '("\\.soy\\'" . closure-template-html-mode))

(defun bk-soy-mode-hook ()
  (set-fill-column 100)
)
(add-hook 'closure-template-html-mode-hook 'bk-soy-mode-hook)

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
  (local-set-key (kbd "<C-return>") 'semantic-complete-analyze-and-replace)
  (local-set-key (kbd "<C-tab>") 'ff-find-other-file)
  (setq skeleton-pair 1)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs server

;; (server-start)
;; (add-hook 'server-switch-hook
;;           (lambda nil
;;             (let ((server-buf (current-buffer)))
;;               (bury-buffer)
;;               (switch-to-buffer-other-frame server-buf))))
;; (add-hook 'server-done-hook 'delete-frame)
;; (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff

(require 'org)
(require 'org-protocol)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-startup-indented t)

(setq org-agenda-files (list "~/org/inbox.org"
                             "~/org/work.org"
                             "~/org/now.org"))
;; A do-nothing function for capturing to current point 
(defun bk-paste-link-template-point ()
  (interactive)
  )
(setq org-capture-templates
      (quote
       (("w"
         "Default template"
         entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)
        ("l"
         "Link template"
         entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* %c %T"
         :empty-lines 1
         :immediate-finish 1)
        ("v"
         "Paste link template"
         plain
         (function bk-paste-link-template-point)
         "[[%c][>>]]"
         :empty-lines 0
         :immediate-finish 1
         :unnarrowed 1)
        ;; ... more templates here ...
        )))

;; Open links in chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; Refile targets
(setq org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 1)
                           ("~/org/done.org" :maxlevel . 1)))

(defun bk-org-agenda-mode-hook()
  (local-set-key (kbd "j") 'org-agenda-next-line)
  (local-set-key (kbd "k") 'org-agenda-previous-line)
  )
(add-hook 'org-agenda-mode-hook 'bk-org-agenda-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; currently not working stuff (need to fix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server stuff

;; (add-hook 'server-switch-hook
;; 	  (lambda nil
;; 	    (let ((server-buf (current-buffer)))
;; 	      (bury-buffer)
;; 	      (switch-to-buffer-other-frame server-buf))))
;; (add-hook 'server-done-hook 'delete-frame)

;; returns t if the current buffer is an emacs-client file
;; (defun is-buffer-a-client ()
;;   (interactive)
;;   (let ((cls server-clients)
;; 	cl
;; 	bufs
;; 	buf
;; 	(ok nil))
;;     (while cls
;;       (setq cl (car cls))
;;       (setq bufs (cdr cl))
;;       (while bufs
;; 	(setq buf (car bufs))
;; 	(if (eq buf (current-buffer))
;; 	    (setq ok t))
;; 	(setq bufs (cdr bufs)))
;;       (setq cls (cdr cls)))
;;     ok))
;; (add-hook 'server-switch-hook 'make-frame-command)
;; (add-hook 'server-done-hook '(lambda ()
;; 			       (interactive)
;;  			       (if (is-buffer-a-client)
;;  				   (delete-frame))))
;; (add-hook 'server-switch-hook
;;             (lambda ()
;;               (when (current-local-map)
;;                 (use-local-map (copy-keymap (current-local-map))))
;; 	      (when server-buffer-clients
;; 		(local-set-key (kbd "C-x k") 'server-edit))))

;; emacsclients frames are popping up behind other windows; fix this
;; (add-hook 'server-visit-hook
;;           (lambda()
;;             (raise-frame)))

;; (defun px-raise-frame-and-give-focus ()
;;   (when window-system
;;     (raise-frame)
;;     (x-focus-frame (selected-frame))
;;     (set-mouse-pixel-position (selected-frame) 4 4)
;;     ))

;; (add-hook 'server-switch-hook
;;           'px-raise-frame-and-give-focus)


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

;; (autoload 'camelCase-mode "camelCase-mode")

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
    ;; (if (not (display-graphic-p))
    ;;     (defvar zenburn-override-colors-alist
    ;;       '(("zenburn-bg"  . "ARGBBB000000")
    ;;         ("zenburn-bk-region-bg" . "ARGBBB000000"))))
    (load-theme 'zenburn t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic

(global-ede-mode 1)
(require 'semantic/sb)

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode) ; Maintain tag database.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) ; Reparse buffer when idle.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) ; Show summary of tag at point.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode) ; Show completions when idle.
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode) ; Additional tag decorations.
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode) ; Highlight the current tag.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) ; Show current fun in header line.
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) ; Provide `switch-to-buffer'-like keybinding for tag names.
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) ; A mouse 3 context menu.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode) ; Highlight references of the symbol under point.
(semantic-mode 1)

(global-set-key '[(C-down-mouse-1)] 'semantic-ia-fast-mouse-jump)


(global-ede-mode 1)
(require 'semantic/sb)

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode) ; Maintain tag database.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) ; Reparse buffer when idle.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) ; Show summary of tag at point.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode) ; Show completions when idle.
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode) ; Additional tag decorations.
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode) ; Highlight the current tag.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) ; Show current fun in header line.
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) ; Provide `switch-to-buffer'-like keybinding for tag names.
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) ; A mouse 3 context menu.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode) ; Highlight references of the symbol under point.
(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (org-edna magithub f magit yasnippet js2-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:foreground "green2" :underline t))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#308430")))))
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el
