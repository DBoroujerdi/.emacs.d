(require 'company)
(require 'cl-lib)

(defun company-imenu ()
  "Company completion for imenu"
  (interactive)
  (company-begin-backend 'company-imenu)

)

(provide 'company-imenu)
;;; company-imenu.el ends here
