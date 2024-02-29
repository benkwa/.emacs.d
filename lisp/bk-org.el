;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff

(require 'org-protocol)
(require 'org-feed)
(require 'org-id)

;; Store links in any buffers
(define-key global-map "\C-cl" 'org-store-link)
;; Call up agenda from any buffer
(define-key global-map "\C-ca" 'org-agenda)
;; Add a new key binding for org-capture
(define-key global-map "\C-cc" 'org-capture)
;; Log timestamps for done items
(setq org-log-done t)
;; Start in indented mode
(setq org-startup-indented t)
;; Right-justify tags to column 80
(setq org-tags-column -80)
;; Always insert new line before bullet; for plain lists, try to DWIM.
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
;; Use path-style refile targets; allow refiling to the top level.
(setq org-refile-use-outline-path 'file)

(setq org-agenda-files (list "~/org"
                             ))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)
        ("~/org/done.org" :maxlevel . 1)
        ("~/org/later.org" :maxlevel . 1)))

(setq org-capture-templates `(
	("p" "Protocol" entry (file+headline ,"~/org/now.org" "Inbox")
         "* %?[[%:link][%:description]] %U\n%i\n"
         :empty-lines 0
         :empty-lines-after 2
         :immediate-finish 1
         :unnarrowed 1)
	("L" "Protocol Link" entry (file+headline ,"~/org/now.org" "Inbox")
         "* %?[[%:link][%:description]] %U"
         :empty-lines 0
         :empty-lines-after 2
         :immediate-finish 1
         :unnarrowed 1)
        ("l" "Paste link from clipboard" plain (function ignore)
         "[[%c][>>]]"
         :empty-lines 0
         :immediate-finish 1
         :no-save 1
         )
        ("j" "Journal" entry (file+datetree, "~/org/journal.org")
         "* %?\n%U\n\n\n"
         :clock-in t
         :clock-resume t
         :empty-lines-after 2)
        ("m" "Meeting notes" entry (file+headline, "~/org/now.org" "Inbox")
         "* %?\n%U\n\n\n"
         :clock-in t
         :clock-resume t
         :empty-lines-after 2)
        )
)

(defun bk-org-agenda-mode-hook ()
  (local-set-key (kbd "j") 'org-agenda-next-line)
  (local-set-key (kbd "k") 'org-agenda-previous-line)
  )
(add-hook 'org-agenda-mode-hook 'bk-org-agenda-mode-hook)

(defun bk-org-mode-hook ()
  (local-set-key (kbd "C-M-n") 'org-forward-heading-same-level)
  (local-set-key (kbd "C-M-p") 'org-backward-heading-same-level)
  (local-set-key (kbd "C-M-u") 'outline-up-heading)
  (local-set-key (kbd "C-M-d") 'outline-next-visible-heading)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  )
(add-hook 'org-mode-hook 'bk-org-mode-hook)
;; Auto fill when typing in org mode
(add-hook 'org-mode-hook 'auto-fill-mode)

(defun bk-org-log-delete ()
  "Delete logbook drawer of subtree."
  (interactive)
  (save-excursion
    (goto-char (org-log-beginning))
    (when (save-excursion
            (save-match-data
              (beginning-of-line 0)
              (search-forward-regexp org-drawer-regexp)
              (goto-char (match-beginning 1))
              (looking-at "LOGBOOK")))
      (org-mark-element)
      (delete-region (region-beginning) (region-end))
      (org-remove-empty-drawer-at (point)))))

(defun bk-org-fix-whitespace ()
  (interactive)
  (save-excursion
    (replace-regexp "^
*\\* " "

* " nil (point-min) (point-max))
    (replace-regexp "^
*\\*\\*" "
**" nil (point-min) (point-max))
    (org-set-tags 1 t)))

(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-handlers
               '("." . browse-url-dwa)))

(defun browse-url-dwa (url &optional _new-window)
  "Ask the Google Chrome WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chrome-arguments' are also passed to
Google Chrome.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "google-chrome " url) nil
	   "/Users/kenobi/bin/google-chrome"
	   (append
	    browse-url-chrome-arguments
            (list "--profile-directory=Profile 8")
	    (list url)))))

(org-link-set-parameters "dwa" :follow (lambda (path) (browse-url-dwa (concat "https:" path))))

(provide 'bk-org)
