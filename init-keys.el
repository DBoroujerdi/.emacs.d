
;;; Code:

(general-define-key
 "S-C-<left>" 'shrink-window-horizontally
 "S-C-<right>" 'enlarge-window-horizontally
 "S-C-<down>" 'shrink-window
 "S-C-<up>" 'enlarge-window)

(global-set-key (kbd "C-c j") 'avy-goto-char)

;; commenting
(global-set-key (kbd "C-x c") 'comment-region)
(global-set-key (kbd "C-x x") 'uncomment-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(provide 'init-keys)
;;; init-keys.el ends here
