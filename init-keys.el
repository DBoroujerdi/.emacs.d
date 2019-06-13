
;;; Code:

(general-define-key
 "S-C-<left>" 'shrink-window-horizontally
 "S-C-<right>" 'enlarge-window-horizontally
 "S-C-<down>" 'shrink-window
 "S-C-<up>" 'enlarge-window)

(global-set-key (kbd "M-j") 'avy-goto-char)

(provide 'init-keys)
;;; init-keys.el ends here
