
;;;

;; M-x describe-bindings
;; C-c C-hで現在のkey bindingsが確認できる

;; M-f 	forward-word 	次の単語へ移動
;; M-b 	backward-word 	前の単語に移動
;; M-d 	kill-word 	単語を削除
;; M-@ 	mark-word 	前の単語をマーク
;; M-a 	backward-sentence 	前の文に移動
;; M-e 	forward-sentence 	次の文に移動
;; M-k 	kill-sentence 	文を削除
;; M-z 	zap-to-char 	指定した文字まで削除 参考
;; M-SPC 	just-one-space 	連続したスペースを一つにまとめる
;; C-M-f 	forward-sexp 	次のS式へ移動
;; C-M-b 	backward-sexp 	前のS式へ移動
;; C-M-n 	forward-list 	次の括弧終わりに移動
;; C-M-p 	backward-list 	前の括弧始まりに移動
;; C-M-a 	c-beginning-of-defun 	関数定義の先頭に移動
;; C-M-e 	c-end-of-defun 	関数定義の終わりに移動
;; C-M-h 	c-mark-function 	関数単位で選択


;; 高速化のための設定
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 10 800000)))) ;; restore after startup
(setq inhibit-startup-screen t)
;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)
;;; splash screenを無効にする
(setq inhibit-splash-screen t)
;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)
;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)


;; straight or use-package
(setq use-straight nil)
(cond
 (use-straight
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 4))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  ;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
  ;; 本来は (use-package hoge :straight t) のように書く必要がある
  (setq straight-use-package-by-default t)
  (straight-use-package 'spacemacs-theme)
  (load-theme 'spacemacs-dark t)
  )
 (t
  (package-initialize)
  (eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20180613.2219/")
  (require 'use-package))
  ;;(use-package diminish)
  (use-package bind-key)
  (require 'spacemacs-dark-theme)
  (load-theme 'spacemacs-dark t)
  ))

;; highlight
(global-hl-line-mode t) ;; 現在行をハイライト
(show-paren-mode t)                       ;; 対応する括弧をハイライト
(setq show-paren-style 'mixed)            ;; 括弧のハイライトの設定。
(transient-mark-mode t)                   ;; 選択範囲をハイライト
(use-package volatile-highlights
  :config (volatile-highlights-mode t))
;; scroll量調整
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1) 
(global-set-key "\C-h" 'delete-backward-char)

(add-to-list 'load-path "~/.emacs.d/elisp")

;; copy&paste using clipboard
(when (equal system-type 'gnu/linux)
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
                  (process-send-eof proc)))))))

;; use markdown-mode for emacs
;; multimarkdownをinstallする必要がある
;; C-c C-c v ブラウザでプレビュー
;; C-c C-c l ewwでpreview
;; C-c C-x Enterでbuffer内で整形表示
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))
(use-package company ;; 補完 company
  :config
  ;; (global-company-mode) ; 全バッファで有効にする
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'markdown-mode-hook 'company-mode)
  (add-hook 'gfm-mode-hook 'company-mode)
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  )
(use-package anzu ;; 検索文字が何番目か教えてくれるanzu
  :config (global-anzu-mode +1))
(with-eval-after-load 'eww
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
  )

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (when (require 'flycheck nil 'noerror)
    (custom-set-variables
     ;; エラーをポップアップで表示
     '(flycheck-display-errors-function
       (lambda (errors)
         (let ((messages (mapcar #'flycheck-error-message errors)))
           (popup-tip (mapconcat 'identity messages "\n")))))
     '(flycheck-display-errors-delay 0.5))
    (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
    (add-hook 'c-mode-common-hook 'flycheck-mode))
  )

;; M-x yas-describe-tablesで現在展開できるテンプレートを表示できる
;; yasnippet-snippetsでsnippets群を入れて使う
(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
   ("C-x i i" . yas-insert-snippet);; 既存スニペットを挿入する
   ("C-x i n" . yas-new-snippet);; 新規スニペットを作成するバッファを用意する
   ("C-x i v" . yas-visit-snippet-file);; 既存スニペットを閲覧・編集する
   ))

(use-package helm
  :init
  (require 'helm)
  (require 'helm-config)
  :bind-keymap
  ("C-c h" . helm-command-prefix)
  :bind (;; helm-show-kill-ring キルリングを見やすい形式で表示してくれて, かつpatternsで絞り込み
         ("M-y". helm-show-kill-ring) 
         ;; helm-miniはpatternsに@を前置することで, バッファ名ではなく
         ;; バッファの内容で絞り込むこともできます.
         ;; helm-mini 例えば, testという文字を含んだバッファを絞り込みたければpatternsを
         ;; @testとすればよいのです. さらにそのバッファ内の何行目にtestという文字が
         ;; 含まれているのかが知りたければ, C-sと入力します. すると,
         ;; helm-miniセッションはhelm-moccurにスイッチされ, 候補が表示されます.
         ("C-x b" . helm-mini)
         ;; helm-find-file C-sでgrepっぽいことができます. C-u C-sでrecursive grepです.
         ;; C-lでひとつ上のディレクトリに移動できます. C-rで戻れます.
         ;; C-sでgrepっぽいことができます.
         ("C-x C-f". helm-find-files)
         ("M-x" . helm-M-x)
         :map helm-map
         ([tab] . helm-execute-persistent-action); rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB work in terminal
         ("C-z" . helm-select-action); list actions using C-z
         :map helm-command-prefix
         ("o" . helm-occur)
         )
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t ; optional fuzzy matching for helm-M-x
        )
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  )
;; ==========================================================================================================
;; C-c h i : helm-semantic-or-imenu (Imenuはいろいろな定義を探す方法を提供します. たとえば関数定義とか変数定義)
;; beginning-of-defun(C-M-a)とend-of-defun(C-M-e)を使えます(関数の前後に飛べて便利)
;; 検索listでtabを押すと、そこまで飛んでくれる。C-mすればそこに着地、C-gすると、検索を始めた時点に戻ってくれる。
;; ==========================================================================================================

(use-package ripgrep
  :config (setq ripgrep-arguments '("-S"))
  :bind ("C-c s" . ripgrep-regexp))
(use-package undo-tree
  :config (global-undo-tree-mode t))
(use-package helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
;; C-c p f ファイル表示
;; プロジェクト内のファイルにGrep(helm-projectile-grep、C-c p s g
(use-package flymd
  :bind ("C-c f" . flymd-flyit))
(use-package magit
  :bind ("C-c g" . magit-status))
(use-package hydra
  :config
  (global-set-key
   (kbd "C-z")
   (defhydra hydra-move ()
     "move"
     ("f" forward-char "right")
     ("b" backward-char "left")
     ("n" next-line "down")
     ("p" previous-line "up")
     ("SPC" scroll-up-command "down")
     ("<backspace>" scroll-down-command "up")
     ("." hydra-repeat "repeat"))))

;; ;;;
