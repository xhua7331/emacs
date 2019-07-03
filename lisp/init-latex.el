;;set the coding format :utf-8
(set-language-environment "UTF-8")

;; 解决显示Unicode字符的卡顿问题
(setq inhibit-compacting-font-caches t)

;; 汉字默认字体为Kaiti(楷体)，可改为其它字体
(set-fontset-font "fontset-default" 'han
		  "KaiTi")
;; 数学符号默认字体为Cambria Math
(set-fontset-font "fontset-default" 'symbol
		  "Cambria Math")

;(add-to-list 'load-path "~/.emacs.d/elpa/auctex-12.1.2")

(require 'cdlatex) ;; 载入 cdlatex
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex);; 告诉 emacs 打开 latex 文档时打开 cdlatex 插件

;; 设定outline minor mode的前缀快捷键为C-o
(setq outline-minor-mode-prefix [(control o)])
(setq-default TeX-master nil) ;; 编译时问询主文件名称
(setq TeX-auto-save t)
(setq TeX-parse-selt t) ;; 对新文件自动解析(usepackage, bibliograph, newtheorem等信息)
(setq TeX-save-query nil) ;;save the modified file when compiling
(setq TeX-engine 'xetex)  ;;set xetex to compile
(setq TeX-show-compilation t) ;display the compilation windows when compiling


;; 打开TeX文件时应该加载的mode/执行的命令
(defun my-latex-hook ()
  (turn-on-cdlatex) ;; 加载cdlatex
  (outline-minor-mode)  ;;加载outline-minor mode
  (turn-on-reftex)  ;; 加载reftex
  (auto-fill-mode) ;;自动换行
  (TeX-fold-mode t) ;; 加载TeX fold mode
  (outline-hide-body) ;; 打开文件时只显示章节标题

  
  ;; 以下两行是正向搜索相关设置
  (assq-delete-all (quote output-pdf) TeX-view-program-selection) 
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))
  )
(add-hook 'LaTeX-mode-hook 'my-latex-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; PDF正向搜索相关设置
(setq TeX-PDF-mode t) 
(setq TeX-source-correlate-mode t) 
(setq TeX-source-correlate-method 'synctex) 
(setq TeX-view-program-list 
 '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n ") " %o")))) 




(provide 'init-latex)
