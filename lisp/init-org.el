
(require 'org);;set org-mode synatic highlighting
(setq org-src-fontify-natively t)

;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/.emacs.d"))



;;设置一个打开gtd的模板
(setq org-capture-templatep
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))




(provide 'init-org)
