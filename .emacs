;;;

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
;; (require 'drill-instructor)
;; (setq drill-instructor-global t)

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
;; (custom-set-faces
;; '(hl-line ((t (:background "color-236")))))
;; (custom-set-faces
;; '(hl-line ((t (:background "color-220")))))


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

;; spacemacs風の見た目にする
(load-theme 'spacemacs-dark t)

;; ewwのdefault検索をgoogleにする
(setq eww-search-prefix "https://www.google.co.jp/search?q=")
;; 普通のewwは背景が白っぽくなるので、白っぽくならないための設定
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))
;; high lighting search result
(defun eww-search (term)
  (interactive "sSearch terms: ")
  (setq eww-hl-search-word term)
  (eww-browse-url (concat eww-search-prefix term)))
(add-hook 'eww-after-render-hook (lambda ()
                   (highlight-regexp eww-hl-search-word)
                   (setq eww-hl-search-word nil)))


;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (when (require 'flycheck nil 'noerror)
;;   (custom-set-variables
;;    ;; エラーをポップアップで表示
;;    '(flycheck-display-errors-function
;;      (lambda (errors)
;;        (let ((messages (mapcar #'flycheck-error-message errors)))
;;          (popup-tip (mapconcat 'identity messages "\n")))))
;;    '(flycheck-display-errors-delay 0.5))
;;   (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
;;   (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
;;   (add-hook 'c-mode-common-hook 'flycheck-mode))

;; yasnippet
;; M-x yas-describe-tablesで現在展開できるテンプレートを表示できる
;; yasnippet-snippetsでsnippets群を入れて使う
(use-package yasnippet)
(yas-global-mode 1)
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;; 検索系
;; helm
(use-package helm-config
  :config (helm-mode 1))
;; helm-ag helmを使った検索  silversearcher-agをinstallしないと使えない
(use-package helm-ag)
;; (setq helm-ag-base-command "ag --nocolor --nogrou")
(setq helm-ag-base-command "rg --vimgrep --no-heading")
;;; C-M-gはちょうどあいてる
(global-set-key (kbd "C-c r") 'helm-ag)
(global-set-key (kbd "C-M-k") 'backward-kill-sexp) ;推奨
;; (global-set-key (kbd "C-c s") 'helm-ag)
;; helm-do-ag (helmがinstallされていれば使える) (M-x helm do agとうつ)
;; (global-set-key (kbd "C-c s") 'helm-do-ag)

;; ripgrep 現状ripgrepが検索最強
(use-package ripgrep)
(setq ripgrep-arguments '("-S"))
(global-set-key (kbd "C-c s") 'ripgrep-regexp)
;;;
