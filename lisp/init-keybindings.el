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
(global-set-key (kbd "C-c p f") 'counsel-git)

;;(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer);;set indent key
(global-set-key (kbd "M-/") 'hippie-expand) ;;set hippie complete

;;set expand-region 
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;; iedit key (default c-; )
;;(global-set-key (kbd "M-s e") 'iedit-mode)



;;;;;;;;;;;org-mode key 
;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)
;; rake remember
(global-set-key (kbd "C-c r") 'org-capture)


;; change company m-n to c-n
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;;auto-snippets
(global-set-key (kbd "H-w") #'aya-create)
(global-set-key (kbd "H-y") #'aya-expand)

;;use c-w to delete a backword
;;(global-set-key (kbd "C-w") 'backward-kill-word)
(provide 'init-keybindings)
