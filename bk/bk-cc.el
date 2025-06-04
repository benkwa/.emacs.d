;;; bk-cc.el --- c-mode stuff                        -*- lexical-binding: t; -*-

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
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
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

  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\[ . ?\])
                              (?\( . ?\))))
  (set-fill-column 120)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
)
(add-hook 'c-mode-common-hook 'bk-c++-mode-hook)
(add-hook 'c-mode-common-hook 'subword-mode)
(add-hook 'c-mode-common-hook 'electric-indent-mode)
(add-hook 'c-mode-common-hook 'electric-pair-mode)
(add-hook 'c-mode-common-hook 'display-fill-column-indicator-mode)

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



(provide 'bk-cc)
