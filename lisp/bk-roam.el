;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam experiments


(defun daily-file (day)
  "Generate a daily file (day) days before today"
  (format-time-string "%Y-%m-%d.org" (- (time-convert nil 'integer) (* 86400 day)))
)

(defun two-weeks ()
  "Generate a list of the last two weeks of daily files"
  (let ((default-directory "~/org-roam/daily"))
    (mapcar #'expand-file-name
            (let (value)
              (dotimes (number 14)
                (setq value (cons (daily-file number) value)))
              value))))

;(two-weeks)
;(seq-filter 'file-exists-p (two-weeks))

(use-package org
  :ensure t
  :custom
  (org-log-done t)
  ;; Start in indented mode
  (org-startup-indented t)
  ;; Right-justify tags to column 80
  (org-tags-column -80)
  ;; Always insert new line before bullet; for plain lists, try to DWIM.
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  ;; Agenda settings
  (org-agenda-files (list "~/org-roam/daily"))
;  (org-agenda-files (two-weeks))

  :bind (("C-c a" . org-agenda)
         :map org-mode-map
              ("C-M-n" . org-forward-heading-same-level)
              ("C-M-p" . org-backward-heading-same-level)
              ("C-M-u" . outline-up-heading)
              ("C-M-d" . outline-next-visible-heading)
         ;; :map org-agenda-mode-map
         ;;      ("j" . org-agenda-next-line)
         ;;      ("k" . org-agenda-previous-line)
              )
  :config
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  :hook ((org-mode . auto-fill-mode))
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
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
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n d" . org-roam-dailies-goto-date)
         )
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template "${title:*} ${tags:55}")

  ;; (setq org-roam-dailies-capture-templates
  ;;       '(
  ;;         ("d" "default" entry "* %?"
  ;;          :target
  ;;          (file+head+olp
  ;;           "%<%Y-%m-%d>.org"fdsfad
  ;;           "#+title: %<%Y-%m-%d>\n"
  ;;           ("TODO sort")))
  ;;         )
  ;;       )

  ;; Place the org buffer in a side window
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.25)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))
  ;; If using org-roam-protocol
  ;(require 'org-roam-protocol)
  )

;; This hack fixes timing issues when making sqlite queries via emacsql.
(defun my/sleep (_1 &optional _2)
  (sleep-for 0 1))
(advice-add 'org-roam-db-query :before 'my/sleep)

;; (defun bk-org-mode-hook ()
;;   (local-set-key (kbd "C-M-n") 'org-forward-heading-same-level)
;;   (local-set-key (kbd "C-M-p") 'org-backward-heading-same-level)
;;   (local-set-key (kbd "C-M-u") 'outline-up-heading)
;;   (local-set-key (kbd "C-M-d") 'outline-next-visible-heading)
;;   (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;;   )
;; (add-hook 'org-mode-hook 'bk-org-mode-hook)
;; (defun bk-org-agenda-mode-hook ()
;;   (local-set-key (kbd "j") 'org-agenda-next-line)
;;   (local-set-key (kbd "k") 'org-agenda-previous-line)
;;   )
;; (add-hook 'org-agenda-mode-hook 'bk-org-agenda-mode-hook)
;; (add-hook 'org-mode-hook 'auto-fill-mode)

;; (require 'org-roam)
;; (setq org-roam-directory "~/org-roam")

(provide 'bk-roam)
