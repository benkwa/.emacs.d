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

(use-package org
  :ensure t

  :custom
  (org-log-done t) ; Log timestamps for done items
  (org-startup-indented t) ; Start in indented mode
  (org-tags-column -80); Right-justify tags to column 80
  ;; Always insert new line before bullet; for plain lists, try to DWIM.
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (org-agenda-files (list "~/org/daily")) ; pull agenda from daily files
  ;;(org-agenda-files (bk-org/two-weeks)) ; experiment: only use last 2 weeks
  (org-persist-directory (file-truename "~/.emacs.d.cache/org-persist"))
  (org-id-locations-file (file-truename "~/.emacs.d.cache/org-id-locations"))
  (org-bookmark-names-plist `(:last-refile "org-refile-last-stored"
                              :last-capture-marker "org-capture-last-stored-marker"))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-M-n" . org-forward-heading-same-level)
   ("C-M-p" . org-backward-heading-same-level)
   ("C-M-u" . outline-up-heading)
   ("C-M-d" . outline-next-visible-heading)
   )

  :config
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  (org-link-set-parameters "jira"
      :follow (lambda (id) (browse-url-chrome (concat "https://dreamworks.atlassian.net/browse/" id)))
      :face 'org-link-jira)
  (org-link-set-parameters "pr"
      :follow (lambda (id) (browse-url-chrome (concat "https://github.com/dwanim/premo/issues/" id)))
      :face 'org-link-jira)
  (org-link-set-parameters "id" :face 'org-link-id)

  (unbind-key "M-<left>" org-mode-map)
  (unbind-key "M-<right>" org-mode-map)

  (defun bk/add-org-link ()
    (interactive)
    (let ((link-node (org-roam-node-read)))
      (if (org-roam-node-id link-node)
          (let ((this-node (org-roam-node-at-point 'assert))
                (link-id (org-roam-node-id link-node))
                (link-title (org-roam-node-title link-node)))
            (save-excursion
              (goto-char (org-roam-node-point this-node))
              (org-roam-property-add "LINKS"
                                     (org-link-make-string (concat "id:" link-id) link-title)))
            )
        (error "no such node")
        )))

  :hook
  ((org-mode . auto-fill-mode))

  :custom-face
  (org-link ((t (:foreground "steelblue1" :underline t)))) ; http/https
  (org-link-id ((t (:foreground "green2" :underline t))))      ; internal org links
  (org-link-jira ((t (:foreground "sienna2" :underline t)))) ; jira tickets
  (org-date ((t (:underline nil))))
  )


(use-package org-roam
  :ensure t

  :custom
  (org-roam-directory (file-truename "~/org"))
  (org-roam-db-location (file-truename "~/.emacs.d.cache/org-roam.db"))

  (org-roam-mode-sections
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         ))

  :bind
  (;; global - navigation
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n k" . (lambda() (interactive) (find-file "~/org/kiv.org"))) ; kiv
   ("C-c n n" . (lambda() (interactive) (find-file "~/org/now.org"))) ; now
   ("C-c n t" . org-roam-dailies-goto-today)
   ("C-c n S-t" . org-roam-dailies-goto-tomorrow)
   ("C-c n y" . org-roam-dailies-goto-yesterday)
   ("C-c n d" . org-roam-dailies-goto-date)

   :map org-mode-map
   ;; navigation
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n M-b" . org-roam-dailies-goto-previous-note)
   ("C-c n M-f" . org-roam-dailies-goto-next-note)
   ("C-c n M-<left>" . org-roam-dailies-goto-previous-note)
   ("C-c n M-<right>" . org-roam-dailies-goto-next-note)
   ;; links
   ("C-c l n" . bk/add-org-link)
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
        ("t" "TODO" entry (file "~/org/now.org")
         "* TODO %?\n%U"
         :empty-lines 1)
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
                                     (no-other-window . t)
                                     (no-delete-other-windows . t)))))
  ;; Place the org buffer in a regular window
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*org-roam\\*"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 0.33)
  ;;                (window-height . fit-window-to-buffer)))
  )


;; This hack fixes timing issues when making sqlite queries via emacsql.
(defun my/sleep (_1 &optional _2)
  (sleep-for 0 1))
(advice-add 'org-roam-db-query :before 'my/sleep)

(provide 'bk-org)
