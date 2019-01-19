;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic experiments

(global-ede-mode 1)
(require 'semantic/sb)

;; Maintain tag database.
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;;Reparse buffer when idle.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)

;;Show summary of tag at point.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

;;Show completions when idle.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;;Additional tag decorations.
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)

;;Highlight the current tag.
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;;Show current fun in header line.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;;Provide `switch-to-buffer'-like keybinding for tag names.
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

;;A mouse 3 context menu.
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;;Highlight references of the symbol under point.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

(semantic-mode 1)

(global-set-key '[(C-down-mouse-1)] 'semantic-ia-fast-mouse-jump)

;; Maybe needed in order for org-mode to work with ede-mode.
(defun bk-ede-org-mode-hook()
  ; ede-minor-mode is not needed, and its key bindings interfere
  (ede-minor-mode 0)
  )
(add-hook 'org-mode-hook 'bk-ede-org-mode-hook)


(provide 'bk-ede)
