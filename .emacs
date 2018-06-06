;; use-packageのinstall(http://cachestocaches.com/2015/8/getting-started-use-package/より拝借)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path "~/.emacs.d/elisp")
;; enter利用を禁止
(require 'drill-instructor)
(setq drill-instructor-global t)
;; C-hをdeleteにする
;; もともとこれを入れてたがdrill-instructorのdelete禁止と競合するので下を使う
;; (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key "\C-h" 'delete-backward-char)

;; copy&paste using clipboard
(setq x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil)

(if (display-graphic-p)
    (progn
      (setq x-select-enable-clipboard t)
      (global-set-key "\C-y" 'x-clipboard-yank))
  (progn
    (setq interprogram-paste-function
	  #'(lambda () (shell-command-to-string "xsel -b -o")))
    (setq interprogram-cut-function
	  #'(lambda (text &optional rest)
	      (let*
		  ((process-connection-type nil)
		   (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
		(process-send-string proc text)
		(process-send-eof proc))))))

(setq scroll-step 1)

;; highlight
(global-hl-line-mode t) ;; 現在行をハイライト
;; ハイライトの色をダーク系にする
(custom-set-faces
'(hl-line ((t (:background "color-236")))))
(custom-set-faces
'(hl-line ((t (:background "color-220")))))


(show-paren-mode t)                       ;; 対応する括弧をハイライト
(setq show-paren-style 'mixed)            ;; 括弧のハイライトの設定。
(transient-mark-mode t)                   ;; 選択範囲をハイライト

;;
;; volatile-highlights
;;
;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)


;; use markdown-mode for emacs
;; multimarkdownをinstallする必要がある
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Japanese
;; uncommented by ueda. beacuse in shell buffer, they invokes mozibake
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq enable-double-n-syntax t)

;; 補完 company
(use-package company
  :config
  (global-company-mode) ; 全バッファで有効にする 
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  )

;; 検索文字が何番目か教えてくれるanzu
(use-package anzu
  :config
  (global-anzu-mode +1))








