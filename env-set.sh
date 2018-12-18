
cp .emacs ~/.emacs
curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb
sudo dpkg -i ripgrep_0.10.0_amd64.deb
sudo apt -o install global

# emacs25に入れ替え
sudo http_proxy=${HTTP_PROXY} https_proxy=${HTTP_PROXY} add-apt-repository ppa:ubuntu-elisp/ppa
sudo http_proxy=${HTTP_PROXY} https_proxy=${HTTP_PROXY} apt-get update
sudo http_proxy=${HTTP_PROXY} https_proxy=${HTTP_PROXY} apt-get install emacs25 cmigemo kazam
sudo http_proxy=${HTTP_PROXY} https_proxy=${HTTP_PROXY} update-alternatives --config emacs

sudo apt -o Acquire::http::proxy=$HTTP_PROXY install xsel





