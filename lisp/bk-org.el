;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff

(require 'org)
(require 'org-protocol)
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

(setq org-tags-column -80)
;; Always insert new line before bullet; for plain lists, try to DWIM.
(setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq org-agenda-files (list "~/org/_inbox.org" 
                             "~/org/home.org"
                             "~/org/ping.org"
                             ))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)
        ("~/org/done.org" :maxlevel . 1)
        ("~/org/later.org" :maxlevel . 1)))

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
        ;; ... more templates here ...
        )))
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun bk-org-agenda-mode-hook ()
  (local-set-key (kbd "j") 'org-agenda-next-line)
  (local-set-key (kbd "k") 'org-agenda-previous-line)
  )

(add-hook 'org-agenda-mode-hook 'bk-org-agenda-mode-hook)
(add-hook 'org-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))



(provide 'bk-org)
