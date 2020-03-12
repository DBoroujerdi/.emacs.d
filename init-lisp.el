(use-package rainbow-delimiters
  :config
  (add-hook 'rainbow-delimiters #'clojure-mode))

;; clojure
(use-package clojure-mode)

(use-package paredit
  :init (progn (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1))))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package cider
;;  :pin melpa-stable
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'clojure-mode))


;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;; (add-hook 'emacs-lisp-mode-hook 'autopair-mode)
;; (add-hook 'emacs-lisp-mode-hook '(lambda()
;;                                   (setq indent-tabs-mode nil)
;;                                   ;; (define-key flyspell-mode-map "\M-\t" nil)
;;                                   (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
;;                                   (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)
;;                                   ))

;; todo move lisp config somewhere else.. it requires packages installed for these hooks to work.
;; elisp config
;; Recompile if .elc exists.
;; (add-hook (make-local-variable 'after-save-hook)
;;           (lambda ()
;;             (byte-force-recompile default-directory)))
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Requires Ispell

;; highlight entire expression
(setq show-paren-style 'expression)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

(provide 'init-golang)
;;; init-lisp.el ends here
