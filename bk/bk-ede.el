;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic experiments

(global-ede-mode 1)
(require 'semantic/sb)
(require 'semantic/ia)

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

;; May be needed in order for org-mode to work with ede-mode.
(defun bk-ede-org-mode-hook()
  ; ede-minor-mode is not needed, and its key bindings interfere
  (ede-minor-mode 0)
  )
(add-hook 'org-mode-hook 'bk-ede-org-mode-hook)

;; if you want to enable support for gnu global
;; (when (cedet-gnu-global-version-check t)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; (when (cedet-ectag-version-check t)
;;   (semantic-load-enable-primary-exuberent-ctags-support))

;; (setq semanticdb-project-roots
;;       (list
;;        "/home/kenobi/work/luna/animation"))

(ede-cpp-root-project "myProject"
           :name "myProject"
           :file "/home/kenobi/work/luna/SConstruct"
           :include-path '("/animation"
                           "/animation/snow/command"
                           )
           :system-include-path '("/usr/include/c++/4.5.1"
                                  "/usr/include/"))


;; TODO ede-project-directories needs to be set
;; '(ede-project-directories (quote ("/home/kenobi/work/luna")))

(provide 'bk-ede)

;;; bk-ede.el ends here
