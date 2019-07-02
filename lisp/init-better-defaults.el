
(setq ring-bell-function 'ignore);;turn off voices when scroll to top/bottom

;; 关闭缩进 (第二天中被去除)
;; (electric-indent-mode -1)
;;(linum-mode t)  ;turn up linum mode
(global-linum-mode t);;turn on line number


(setq make-backup-files nil);;禁止生成备份文件
(setq auto-save-default nil) ;;关闭自动保存文件
;;(setq create-lockfiles nil)

(require 'recentf);;recentf mode
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;;(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; 这个快捷键绑定可以用之后的插件 counsel 代替

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode);;show match parent

(global-auto-revert-mode 1);;emacs自动加载外部修改过的文件

;;缩写补全
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; Shifu
					    ("8zl" "zilongshanren")
					    ;; Tudi
					    ;; ("8lxy" "lixinyang")
					    ("8xh" "xhua")
					    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;indent  region or buffer

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)));;point to a buffer top and bottom

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion            ;;save the curse position 
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indent selected region."))
      (progn
	(indent-buffer)
	(message "Indent buffer.")))))

;;;;;;;;;;;;;;;; hippie complete

(setq hippie-expand-try-function-list '(try-expand-debbrev
					try-expand-debbrev-all-buffers
					try-expand-debbrev-from-kill
					try-complete-file-name-partially
					try-complete-file-name
					try-expand-all-abbrevs
					try-expand-list
					try-expand-line
					try-complete-lisp-symbol-partially
					try-complete-lisp-symbol))


(provide 'init-better-defaults)
