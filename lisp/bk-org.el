;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff

(require 'org)
(require 'org-protocol)
(require 'org-feed)

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
        ("v"
         "Paste link template"
         plain
         (function (lambda () nil)) ; do-nothing function for capturing at current point
         "[[%c][>>]]"
         :empty-lines 0
         :immediate-finish 1
         :unnarrowed 1)
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

(defun bk-org-mode-hook ()
  (local-set-key (kbd "C-M-n") 'org-forward-heading-same-level)
  (local-set-key (kbd "C-M-p") 'org-backward-heading-same-level)
  (local-set-key (kbd "C-M-u") 'outline-up-heading)
  (local-set-key (kbd "C-M-d") 'outline-next-visible-heading)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  )
(add-hook 'org-mode-hook 'bk-org-mode-hook)

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

(setq bk-rtm-template
      "
* %title [[%link][>>]] %tags
%(if (not (string-blank-p \"%duedate\")) (concat \"SCHEDULED: \" \"%duedate\n\"))")

(defun create-rtm-feed (name url)
  (add-to-list 'org-feed-alist
               (list (concat "rtm-" name)
                     url
                     "~/org/rmilk.org"
                     name
                     :parse-feed 'org-feed-parse-atom-feed
                     :parse-entry 'bk-parse-rtm-entry
                     :template bk-rtm-template)))

;; Read URLs, and turn them into org feeds.
(if (file-exists-p "~/bk/org-feeds")
    (dolist (feed
             (with-temp-buffer
               (insert-file-contents "~/bk/org-feeds")
               (split-string (buffer-string) "\n" t)))
      (apply 'create-rtm-feed (split-string feed))))

(defun rtm-to-org-date (date-string)
  (let* ((time (parse-time-string date-string))
         (dd (nth 3 time))
         (mm (nth 4 time))
         (yy (nth 5 time)))
    (if (and dd mm yy)
        (format "<%d-%02d-%02d>" yy mm dd)
      "")
))

(defun rtm-to-org-tags (tags)
  (if (string-equal "none" tags)
      ":rtm:"
    (concat ":rtm:" (string-join (split-string tags "," t "[[:space:]]") ":") ":")))

(defun find-child-with-key(elt cls)
  (string= cls (cdr (assq 'class (xml-node-attributes elt)))))

(defun rtm-extract-value (attrs key)
  (let* ((divs (xml-get-children attrs 'div))
         (my-div (seq-find (lambda (elt) (find-child-with-key elt key)) divs))
         (spans (xml-get-children my-div 'span))
         (my-span (seq-find (lambda (elt) (find-child-with-key elt (concat key "_value"))) spans))
         )
    (car (xml-node-children my-span))
    )
  )

(defun bk-parse-rtm-entry (entry)
  "Parse the `:item-full-text' as a sexp and create new properties."
  (let ((xml (car (read-from-string (plist-get entry :item-full-text)))))
    ;; Get first <link href='foo'/>.
    (setq entry (plist-put entry :link
			   (xml-get-attribute
			    (car (xml-get-children xml 'link))
			    'href)))
    ;; Add <title/> as :title.
    (setq entry (plist-put entry :title
			   (xml-substitute-special
			    (car (xml-node-children
				  (car (xml-get-children xml 'title)))))))
    (message "processing %s" (plist-get entry :title))

    (let* ((content (car (xml-get-children xml 'content)))
	   (type (xml-get-attribute-or-nil content 'type)))
      (when content
	(cond
	 ((string= type "text")
	  ;; We like plain text.
	  (setq entry (plist-put entry :description
				 (xml-substitute-special
				  (car (xml-node-children content))))))
	 ((string= type "html")
	  ;; TODO: convert HTML to Org markup.
	  (setq entry (plist-put entry :description
				 (xml-substitute-special
				  (car (xml-node-children content))))))
	 ((string= type "xhtml")
	  ;; TODO: convert XHTML to Org markup.
	  (setq entry (plist-put entry :description
	        		 (prin1-to-string
	        		  (xml-node-children content))))
          (let ((attrs (car (xml-get-children content 'div))))
            (setq entry (plist-put entry :duedate
                                   (rtm-to-org-date (rtm-extract-value attrs "rtm_due"))))
            (setq entry (plist-put entry :tags
                                   (rtm-to-org-tags (rtm-extract-value attrs "rtm_tags"))))
          ))
	 (t
	  (setq entry (plist-put entry :description
				 (format-message
                                  "Unknown `%s' content." type)))))))

    (message "done processing %s" (plist-get entry :title))
    entry))


(provide 'bk-org)
