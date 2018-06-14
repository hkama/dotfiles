;;;

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

;; scroll量調整
(setq scroll-conservatively 35
scroll-margin 0
scroll-step 1) 

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

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; use markdown-mode for emacs
;; multimarkdownをinstallする必要がある
;; (use-package markdown-mode
;;   :ensure t
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown"))

;; Japanese
;; uncommented by ueda. beacuse in shell buffer, they invokes mozibake

;; (set-language-environment 'Japanese)
;; (prefer-coding-system 'utf-8)
;; (setq enable-double-n-syntax t)

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

;; helm
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)
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
;; ==========================================================================================================
;; 上に書いた設定にて、prefixがC-c hとされていて、
;; C-c h /でhelm-find(Unixのfindコマンド)
;; C-c h iでhelm-semantic-or-imenu (Imenuはいろいろな定義を探す方法を提供します. たとえば関数定義とか変数定義)
;; beginning-of-defun(C-M-a)とend-of-defun(C-M-e)を使えます(関数の前後に飛べて便利)
;; 検索listでtabを押すと、そこまで飛んでくれる。C-mすればそこに着地、C-gすると、検索を始めた時点に戻ってくれる。
;; ==========================================================================================================
;; キルリングを見やすい形式で表示してくれて, かつpatternsで絞り込みが出来るようになる
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; helm-mini
;; helm-miniはpatternsに@を前置することで, バッファ名ではなくバッファの内容で絞り込むこともできます.
;; 例えば, testという文字を含んだバッファを絞り込みたければpatternsを@testとすればよいのです. さらにそのバッファ内の何行目にtestという文字が含まれているのかが知りたければ, C-sと入力します. すると, helm-miniセッションはhelm-moccurにスイッチされ, 候補が表示されます.
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
;; helm find file
;; C-lでひとつ上のディレクトリに移動できます. C-rで戻れます.
;; C-sでgrepっぽいことができます. C-u C-sでrecursive grepです. 
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; helm-man-woman
;; C-c h mでmanを即座に検索
;; helm-occur
;; カレントバッファ内でpatternと一致する行を列挙してくれます
(global-set-key (kbd "C-c h o") 'helm-occur)
;; helm-apropos Helmのコマンド・関数・変数などなどEmacsのありとあらゆるオブジェクトのマニュアルを参照するコマンド
;; C-c h a

;; ripgrep 現状ripgrepが検索最強
(use-package ripgrep)
(setq ripgrep-arguments '("-S"))
(global-set-key (kbd "C-c s") 'ripgrep-regexp)

;; helm M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t))

;; helm-projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;; C-c p f ファイル表示
;; プロジェクト内のファイルにGrep(helm-projectile-grep、C-c p s g





;;;
