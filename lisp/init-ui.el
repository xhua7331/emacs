
(tool-bar-mode -1);;close tool bar
(scroll-bar-mode -1);;close up scroll bar
(setq inhibit-splash-screen 1) ;;close boostup figure

(setq-default cursor-type 'bar);;change the cursor style

(global-hl-line-mode t);;highlight selected line
(load-theme 'monokai t) ;;config emacs theme

;;(delete-selection-mode t);;double clicks to replace
(setq initial-frame-alist (quote ((fullscreen . maximized))));;open with fullscreen


(provide 'init-ui)
