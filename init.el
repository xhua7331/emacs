;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)

 ;; Add Packages
(defvar my/packages '(
		      ;;---edit latex file in emacs
		        auctex
		;; --- Auto-completion ---
		       company
	         ;; --- Themes ---
		       monokai-theme
		 ;; --- Better Editor ---
		       hungry-delete
		       swiper
		       counsel ;; together with swiper
		       smartparens
		       ;; ---improve the help windows
		       popwin
		;; ---enhance m-x ---
		  ;;     smex
		;; --- Better Editor ---
	;;	hungry-delete
	;;	swiper
	;;	counsel
	;;	smartparens
		;; --- Major Mode ---
	;;	js2-mode
		;; --- Minor Mode ---
	;;	nodejs-repl
	;;	exec-path-from-shell
		;; --- Themes ---
	;;	monokai-theme
		;; solarized-theme
		) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;解决中文字体卡顿
(setq inhibit-compacting-font-caches t)
;; 设置垃圾回收，在Windows下，emacs25版本会频繁出发垃圾回收，所以需要设置
(when (eq system-type 'windows-nt)
  (setq gc-cons-threshold (* 512 1024 1024))
      (setq gc-cons-percentage 0.5)
      (run-with-idle-timer 5 t #'garbage-collect)
      ;; 显示垃圾回收信息，这个可以作为调试用
      ;; (setq garbage-collection-messages t)
      )


(tool-bar-mode -1);;close tool bar
(scroll-bar-mode -1);;close up scroll bar
(global-linum-mode t)

;; 关闭缩进 (第二天中被去除)
;; (electric-indent-mode -1)
;;(linum-mode t)  ;turn up linum mode
(setq inhibit-splash-screen 1)

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )



(global-set-key (kbd "<f2>" ) 'open-my-init-file);f2 to open the initial file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;daytwo
(global-company-mode 1);;turn up company补全插件
(setq-default cursor-type 'bar);;change the cursor style

(setq make-backup-files nil);;禁止生成备份文件

(require 'org);;set org-mode synatic highlighting
(setq org-src-fontify-natively t)

(require 'recentf);;recentf mode
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;;(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; 这个快捷键绑定可以用之后的插件 counsel 代替
 

(delete-selection-mode t);;double clicks to replace
(setq initial-frame-alist (quote ((fullscreen . maximized))));;open with fullscreen
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode);;show match parents
(global-hl-line-mode t);;highlight selected line
(load-theme 'monokai t) ;;config emacs theme
;;config for hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

;; configure for smex		       
;;(require 'smex) ; Not needed if you use package.el
;;(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;(global-set-key (kbd "M-x") 'smex)
         
;; config for swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

;;config the smartparens mode
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode);;useing in emacs lisp 

;;learn more about emacs 
;;(global-set-key (kbd "c-h c-f")'find-function)
;;(global-set-key (kbd "c-h c-v") 'find-variable)
;;(global-set-key (kbd "c-h c-k") 'find-function-on-key)

;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;daythree
(require 'popwin)
(popwin-mode 1)

;;缩写补全
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; Shifu
					    ("8zl" "zilongshanren")
					    ;; Tudi
					    ;; ("8lxy" "lixinyang")
					    ("8xh" "xhua")
					   ))





;;;;;;;;;;;;;;;;;self_customized



;;;;;;;;;;;;;;;;;;;;;;;;;installed software
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company auctex))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default))))
