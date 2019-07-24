
;;;;;;;;;;;;;;;;;;;;;;;;;installed software
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 143 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cdlatex-command-alist
   (quote
    (("nd" "use nd replace node" "(?) node[anchor=west] {$  $ }" cdlatex-position-cursor nil t t)
     ("nexm" "use nexm replace exm" "\\begin{nexm}{?}{}

\\end{nexm}" cdlatex-position-cursor nil t nil)
     ("sv" "use sv to replace shaded and verbatim environment" "\\begin{shaded}
\\begin{verbatim}
?
\\end{verbatim}
\\end{shaded}" cdlatex-position-cursor nil t t)
     ("mn" "use mn to replace minipage" "\\begin{minipage}{? \\linewidth}

\\end{minipage}" cdlatex-position-cursor nil t t)
     ("exm" "use exm to replace two minipage" "\\begin{exm}{?}{}

\\end{exm}" cdlatex-position-cursor nil t nil))))
 '(cdlatex-paired-parens "$[{(")
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 3)
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(evil-leader/leader "SPC")
 '(evil-want-C-u-scroll t)
 '(org-pomodoro-length 40)
 '(org-pomodoro-long-break-length 45)
 '(package-selected-packages
   (quote
    (evil-nerd-commenter evil-surround powerline window-numbering evil-leader evil auctex company monokai-theme hungry-delete swiper counsel smartparens popwin)))
 '(popwin:popup-window-position (quote right))
 '(popwin:popup-window-width 50)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
