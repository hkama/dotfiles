autoload -U compinit
compinit

PROMPT='%m:%c %n$ '

# 履歴ファイルの保存先
export HISTFILE=${HOME}/.zhistory
# メモリに保存される履歴の件数
export HISTSIZE=1000
# 履歴ファイルに保存される履歴の件数
export SAVEHIST=100000
# # 重複を記録しない
# setopt hist_ignore_dups
# # 開始と終了を記録
# setopt EXTENDED_HISTORY
# # ヒストリに追加されるコマンド行が古いものと同じなら古いものを削除
# setopt hist_ignore_all_dups
# # スペースで始まるコマンド行はヒストリリストから削除
# setopt hist_ignore_space
# # ヒストリを呼び出してから実行する間に一旦編集可能
# setopt hist_verify
# # 余分な空白は詰めて記録
# setopt hist_reduce_blanks
# # 古いコマンドと同じものは無視
# setopt hist_save_no_dups
# # historyコマンドは履歴に登録しない
# setopt hist_no_store
# # 補完時にヒストリを自動的に展開
# setopt hist_expand
# # 履歴をインクリメンタルに追加
# setopt inc_append_history
# historyを共有
# インクリメンタルからの検索
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
# Emacs ライクな操作を有効にする（文字入力中に Ctrl-F,B でカーソル移動など）
# Vi ライクな操作が好みであれば `bindkey -v` とする
bindkey -e

# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、ディレクトリに cd する
# 例： /usr/bin と入力すると /usr/bin ディレクトリに移動
setopt auto_cd

# ↑を設定すると、 .. とだけ入力したら1つ上のディレクトリに移動できるので……
# 2つ上、3つ上にも移動できるようにする
alias ...='cd ../..'
alias ....='cd ../../..'

# "~hoge" が特定のパス名に展開されるようにする（ブックマークのようなもの）
# 例： cd ~hoge と入力すると /long/path/to/hogehoge ディレクトリに移動
hash -d hoge=/long/path/to/hogehoge

# cd した先のディレクトリをディレクトリスタックに追加する
# ディレクトリスタックとは今までに行ったディレクトリの履歴のこと
# `cd +<Tab>` でディレクトリの履歴が表示され、そこに移動できる
setopt auto_pushd

# pushd したとき、ディレクトリがすでにスタックに含まれていればスタックに追加しない
setopt pushd_ignore_dups

# 拡張 glob を有効にする
# glob とはパス名にマッチするワイルドカードパターンのこと
# （たとえば `mv hoge.* ~/dir` における "*"）
# 拡張 glob を有効にすると # ~ ^ もパターンとして扱われる
# どういう意味を持つかは `man zshexpn` の FILENAME GENERATION を参照
setopt extended_glob

# 入力したコマンドがすでにコマンド履歴に含まれる場合、履歴から古いほうのコマンドを削除する
# コマンド履歴とは今まで入力したコマンドの一覧のことで、上下キーでたどれる
setopt hist_ignore_all_dups

# コマンドがスペースで始まる場合、コマンド履歴に追加しない
# 例： <Space>echo hello と入力
setopt hist_ignore_space

# <Tab> でパス名の補完候補を表示したあと、
# 続けて <Tab> を押すと候補からパス名を選択できるようになる
# 候補を選ぶには <Tab> か Ctrl-N,B,F,P
zstyle ':completion:*:default' menu select=1

# 単語の一部として扱われる文字のセットを指定する
# ここではデフォルトのセットから / を抜いたものとする
# こうすると、 Ctrl-W でカーソル前の1単語を削除したとき、 / までで削除が止まる
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

#zman hogeでmanを参照できる
function zman() {
    PAGER="less -g -s '+/^       "$1"'" man zshall
}

# 色を使用
autoload -Uz colors
colors

# 文字コードの指定
export LANG=ja_JP.UTF-8

# 同時に起動したzshの間でヒストリを共有
setopt share_history

export PATH="/home/kamada/anaconda3/bin:$PATH"

# emacsのshellでの文字化け対策
if [[ $TERM = dumb ]]; then
  unset zle_bracketed_paste
fi

# 輝度調整
xgamma -gamma 0.5
