 ;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")

;;add more personal func and place to the top




(require 'init-func)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-latex)
(require 'init-org)
(require 'init-keybindings)
(require 'init-solvequestions) ;;solve questions 

;;;;;;;;;require custom.el
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

;; let custom.el activated
(load-file  custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;daytwo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;daythree

;;;;;;;;;;;;;;;;;self_customized


