;;; package --- Summary
;;; Commentary:
;;; Code:

(defun reload-emacs-config ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(general-def 'normal
  "SPC r r" 'reload-emacs-config)

(defun json-flatten-print ()
  (interactive)
  (unless mark-active
    (error "No region selected"))
  (let ((begin (region-beginning))
        (end (region-end)))
    (kill-region begin end)
    (let ((json-encoding-pretty-print nil))
      (insert (json-encode (json-read-from-string (current-kill 0)))))))

;; (sit-for 0)

(defun diff-buffers (buffer-A buffer-B)
  "Diff two buffers."
  (interactive
   (let* ((only-two? (eq 2 (count-windows)))
          (wins (sort (window-list)
                      (lambda (a b) (< (window-use-time a)
                                       (window-use-time b)))))
          (b1 (if only-two?
                  (window-buffer (-first wins))
                (read-buffer "Buffer A to compare")))
          (b2 (if only-two?
                  (window-buffer (-second-item wins))
                (read-buffer "Buffer B to compare"))))
     (list b1 b2)))
  (let ((old "/tmp/old-diff")
        (new "/tmp/new-diff"))
    (with-temp-file new
      (insert-buffer-substring buffer-A))
    (with-temp-file old
      (insert-buffer-substring buffer-B))
    (diff old new "-u" t)))


(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

(provide 'init-functions)
;;; init-functions.el ends here
