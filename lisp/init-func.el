;;define a initial file and set key to open init.el
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(provide 'init-func)
