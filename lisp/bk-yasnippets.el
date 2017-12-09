;;; bk-yasnippets.el --- lisp code supporting yasnippets
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/yasnippet-0.8.0"))
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bkyas-namespace-from-ctor()
  "Derive the namespace for the current function from the last constructor."
  (save-excursion
    (search-backward "@constructor")
    (search-forward "function(")
    (while (js2-continued-expression-p)
      (back-to-indentation)
      (js2-backward-sws))
    (back-to-indentation)
    (looking-at "^[a-zA-Z.]*")
    (match-string-no-properties 0)))

(defun bkyas-private(function-name)
  "Produce an @private tag if the given function is private."
  (if (not (string= "" function-name))
      (if (string= "_" (substring function-name -1))
          "\n * @private")))

(defun bkyas-param-list(param-list)
  "Turn a param list into a list of @param jsdoc tags."
  (if (not (string= "" param-list))
      (mapconcat '(lambda (p) (concat "\n * @param {?} " p))
                 (split-string param-list ", ")
                 "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commence hacky WIP stuff.

(defun bkyas-find-enclosing-function()
  "Find the enclosing function for the current point."
  (save-excursion
    (search-backward-regexp "^[a-zA-Z._]+")
    (match-string-no-properties 0)))

(defun camelize(s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))


(defun make-pulsar-namespace()
  "Derive the namespace of the current file from its path."
  (let ((package
         (mapconcat 'identity
           (cdr (member "src"
                        (split-string
                         (file-name-directory
                          (file-name-sans-extension buffer-file-name))
                         "/")))
           "."))
        (class (camelize (file-name-nondirectory
                          (file-name-sans-extension buffer-file-name)))))
    (concat "pulsar." package class ".prototype")))

(provide 'bk-yasnippets)
;;; bk-yasnippets.el ends here
