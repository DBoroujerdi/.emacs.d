(defun json-flatten-print ()
  (interactive)
  (unless mark-active
    (error "No region selected."))
  (let ((begin (region-beginning))
        (end (region-end)))
    (kill-region begin end)
    (let ((json-encoding-pretty-print nil))
      (insert (json-encode (json-read-from-string (current-kill 0)))))))

;; (sit-for 0)


(provide 'init-functions)
