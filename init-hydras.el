;;; package --- Summary
;;; Commentary:
;;; Code:

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(defhydra hydra-window-resize (:color red
                                      :hint nil)
  "
 Shrink horizontal: _l_
 Enlarge horizontal: _h_
 Shrink vertically: _j_
 Enlarge vertically: _k_
"
  ("l" 'shrink-window-horizontally)
  ("h" 'enlarge-window-horizontally)
  ("j" 'shrink-window)
  ("k" 'enlarge-window))

(provide 'init-hydras)
;;; init-hydras.el ends here
