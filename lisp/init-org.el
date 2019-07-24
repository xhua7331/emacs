
(require 'org);;set org-mode synatic highlighting
(setq org-src-fontify-natively t)

;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/.emacs.d/org"))



;;设置一个打开gtd的模板
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/org/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))

(add-to-list 'org-capture-templates
             '("j" "Journal" entry (file+datetree "~/.emacs.d/org/journal.org")
               "* %U - %^{heading}\n  %?"))

(provide 'init-org)
