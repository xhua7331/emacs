


;;;;;;;;;;;;;;;;;;;;;;;set packages resource
(when (>= emacs-major-version 24)
     (require 'package)
    ;; (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)


;; Add Packages
(defvar xhua7331/packages '(
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
			    expand-region
			    iedit
;;;;python environment config
			    elpy
			    flycheck
			    py-autopep8

			    ;;org-pomodoro
			     org-pomodoro
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

(setq package-selected-packages xhua7331/packages)

(defun xhua7331/packages-installed-p ()
  (loop for pkg in xhua7331/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (xhua7331/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg xhua7331/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-company-mode 1);;turn up company补全插件


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


;;config the smartparens mode
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode);;useing in emacs lisp

;;config popwin mode
(require 'popwin)
(popwin-mode 1)

;;set expand-region key
(require 'expand-region)

;; ;; config python environment
(elpy-enable)
;;(elpy-use-ipython)
;; (setq-default c-basic-offset   4
;;               tab-width        4
;;               indent-tabs-mode nil)
        (setq tab-width 4)
        (set-variable 'python-indent-offset 4)
        (set-variable 'python-indent-guess-indent-offset nil)


;;autopep8设置
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)


;;config flychek
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;;(global-flycheck-mode)
(require 'org-pomodoro)

(provide 'init-packages)
