;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff


(defun bk-org/daily-file (day)
  "Generate a daily file (day) days before today"
  (format-time-string "%Y-%m-%d.org" (- (time-convert nil 'integer) (* 86400 day)))
)

(defun bk-org/two-weeks ()
  "Generate a list of the last two weeks of daily files"
  (let ((default-directory "~/org/daily"))
    (mapcar #'expand-file-name
            (let (value)
              (dotimes (number 14)
                (setq value (cons (bk-org/daily-file number) value)))
              value))))

(defun bk-org/browse-url (url &optional _new-window)
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
           "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
	   (append
	    browse-url-chrome-arguments
            (list "--profile-directory=Profile 1")
	    (list url)))))
(with-eval-after-load 'browse-url
  (add-to-list 'browse-url-handlers
               '("." . bk-org/browse-url)))

(use-package org
  :ensure t

  :custom
  ;; Log timestamps for done items
  (org-log-done t)
  ;; Start in indented mode
  (org-startup-indented t)
  ;; Right-justify tags to column 80
  (org-tags-column -80)
  ;; Always insert new line before bullet; for plain lists, try to DWIM.
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  ;; Agenda settings
   (org-agenda-files (list "~/org/daily"))
;  (org-agenda-files (bk-org/two-weeks))
  (org-persist-directory (file-truename "~/.emacs.cache/org-persist"))
;  (org-id-locations-file (file-truename "~/.emacs.cache/org-id-locations"))
  (org-bookmark-names-plist `(:last-refile "org-refile-last-stored"
                              :last-capture-marker "org-capture-last-stored-marker"))

  :bind (("C-c a" . org-agenda)
         :map org-mode-map
              ("C-M-n" . org-forward-heading-same-level)
              ("C-M-p" . org-backward-heading-same-level)
              ("C-M-u" . outline-up-heading)
              ("C-M-d" . outline-next-visible-heading)
              ("C-c c" . org-capture)
         ;; :map org-agenda-mode-map
         ;;      ("j" . org-agenda-next-line)
         ;;      ("k" . org-agenda-previous-line)
              )

  :config
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (org-link-set-parameters "dwa" :follow (lambda (path) (bk-org/browse-url (concat "https:" path))))
  (unbind-key "M-<left>" org-mode-map)
  (unbind-key "M-<right>" org-mode-map)
  :hook ((org-mode . auto-fill-mode))
  )


;; (defun save-after-capture-refile ()
;;   (with-current-buffer (marker-buffer org-capture-last-stored-marker)
;;     (save-buffer)))

;; (defun bk-org-mode-hook ()
;;   (advice-add 'org-capture-refile :after 'save-after-capture-refile)
;; )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-db-location (file-truename "~/.emacs.cache/org-roam.db"))

  (org-roam-mode-sections
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n n" . org-roam-dailies-goto-tomorrow)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n d" . org-roam-dailies-goto-date)
         ("C-c n M-b" . org-roam-dailies-goto-previous-note)
         ("C-c n M-f" . org-roam-dailies-goto-next-note)
         ("C-c n M-<left>" . org-roam-dailies-goto-previous-note)
         ("C-c n M-<right>" . org-roam-dailies-goto-next-note)
         )
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template "${title:*} ${tags:55}")


  (setq org-roam-dailies-capture-templates
        '(
          ("d" "default" entry "* %?"
           :target
           (file+head
            "%<%Y-%m-%d>.org"
            "#+title: %<%Y-%m-%d>\n\n* TODO sort"))
          )
        )

  (setq org-capture-templates
        `(
          ("l" "Paste link from clipboard" plain (function ignore)
           "[[%c][>>]]"
           :empty-lines 0
           :immediate-finish 1
           :no-save 1)
          )
        )


  ;; Place the org buffer in a side window
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.25)
               (window-parameters . (
                                     ;; For now, try putting the side window in the window rotation
                                     ;;(no-other-window . t)
                                     (no-delete-other-windows . t)))))
  ;; Place the org buffer in a regular window
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*org-roam\\*"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 0.33)
  ;;                (window-height . fit-window-to-buffer)))


  ;; If using org-roam-protocol
  ;(require 'org-roam-protocol)
  )


;; This hack fixes timing issues when making sqlite queries via emacsql.
(defun my/sleep (_1 &optional _2)
  (sleep-for 0 1))
(advice-add 'org-roam-db-query :before 'my/sleep)


;; (setq org-capture-templates `(
;; 	("p" "Protocol" entry (file+headline ,"~/org/now.org" "Inbox")
;;          "* %?[[%:link][%:description]] %U\n%i\n"
;;          :empty-lines 0
;;          :empty-lines-after 2
;;          :immediate-finish 1
;;          :unnarrowed 1)
;; 	("L" "Protocol Link" entry (file+headline ,"~/org/now.org" "Inbox")
;;          "* %?[[%:link][%:description]] %U"
;;          :empty-lines 0
;;          :empty-lines-after 2
;;          :immediate-finish 1
;;          :unnarrowed 1)
;;         ("l" "Paste link from clipboard" plain (function ignore)
;;          "[[%c][>>]]"
;;          :empty-lines 0
;;          :immediate-finish 1
;;          :no-save 1
;;          )
;;         ("j" "Journal" entry (file+datetree, "~/org/journal.org")
;;          "* %?\n%U\n\n\n"
;;          :clock-in t
;;          :clock-resume t
;;          :empty-lines-after 2)
;;         ("m" "Meeting notes" entry (file+headline, "~/org/now.org" "Inbox")
;;          "* %?\n%U\n\n\n"
;;          :clock-in t
;;          :clock-resume t
;;          :empty-lines-after 2)
;;         )
;; )


;; (defun bk-org-fix-whitespace ()
;;   (interactive)
;;   (save-excursion
;;     (replace-regexp "^
;; *\\* " "

;; * " nil (point-min) (point-max))
;;     (replace-regexp "^
;; *\\*\\*" "
;; **" nil (point-min) (point-max))
;;     (org-set-tags 1 t)))



(provide 'bk-org)
