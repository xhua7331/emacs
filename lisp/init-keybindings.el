(global-set-key (kbd "<f2>" ) 'open-my-init-file);f2 to open the initial file

;;;;;;;;;;;set swiper keybindings
  (global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)


;;learn more about emacs 
;;(global-set-key (kbd "c-h c-f")'find-function)
;;(global-set-key (kbd "c-h c-v") 'find-variable)
;;(global-set-key (kbd "c-h c-k") 'find-function-on-key)


(provide 'init-keybindings)
