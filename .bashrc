

defaults write -g KeyRepeat -int 1
defaults write -g InitialKeyRepeat -int 12

export PROFILE_NAME=default
export USER_NAME=hitoshi.kamada@woven-planet.global
# TJL3
# export APP_LINK=https://tri-ad.okta.com/home/amazon_aws/0oaahkljxpPB8StyA357/272
export APP_LINK=https://tri-ad.okta.com/home/amazon_aws/0oa8x0rn4kIgDdsKY357/272
function saml-configure () {
  saml2aws configure --idp-provider Okta -a ${PROFILE_NAME} --profile ${PROFILE_NAME} --username ${USER_NAME} --url ${APP_LINK} --session-duration=28800 --mfa=Auto --skip-prompt
}

# flutter
export PATH="$PATH:$HOME/flutter/bin"
export PATH="$PATH":"$HOME/.pub-cache/bin"
alias xopen='open -a /Applications/Xcode.app'
[[ -d ~/.rbenv  ]] && \
  export PATH=${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

#aws
# export AWS_DEFAULT_PROFILE=hbu
export AWS_DEFAULT_PROFILE=default
# for vision ai front end app
# export AWS_PROFILE=hbu
export AWS_PROFILE=default

function port_forward() {
    REMOTE_PORT=$3
    LOCAL_PORT=$2
    lsof_return=$(lsof -i:$LOCAL_PORT)
    if [ "$(echo $lsof_return | grep 'LISTEN')" ]; then
        echo "Already port forwarding."
    else
        echo "Start port forwarding."
        echo "ssh -L ${LOCAL_PORT}:localhost:${REMOTE_PORT} $1 -N -f"
        ssh -L ${LOCAL_PORT}:localhost:${REMOTE_PORT} $1 -N -f
    fi
}

# for docker BuildKit
export DOCKER_BUILDKIT=1

# function report_call() {
#     line=$(lsof -i:$2 | tail -n 1)
#     PID_num=$(cut -d' ' -f 2 <<<${line})
#     echo "num"${PID_num}
#     kill ${PID_num}

#     port_forward $1 $2 $3
# }

# function xpro() {
#     aws ssm start-session --target i-0df55ded7a3624b4a --document-name AWS-StartPortForwardingSession --parameters "portNumber=3389, localPortNumber=13389"
# }

# function report() {
#     # report_call ubuntu 9021 9021

#     report_call ubuntu 5901 5900
#     report_call ubuntu 5902 5901
#     report_call ubuntu 5904 5902
#     report_call ubuntu 8988 8988
#     # report_call mlpc 5902 5900
#     report_call hbu    6901 5901

#     # for XProtect
#     # aws ssm start-session --target i-0df55ded7a3624b4a --document-name AWS-StartPortForwardingSession --parameters "portNumber=3389, localPortNumber=13389" &
# }

