# shinjirecs
application for recording TV by using PT3

https://docs.haskellstack.org/en/stable/README/
# Mac OS X
% brew install haskell-stack
% brew install mysql
% brew install postgresql


% stack new shinjirecs-api
% stack setup
% stack path
% stack exec env
% stack build
% stack install hoogle
% stack hoogle --setup
% stack hoogle generate
% export PATH="$HOME/.local/bin:$PATH"
% hoogle server --port 30000
# GHCiのコマンド群
% ghci> :browse! Data.Maybe


# Elm ( Mac OS X )
% https://www.npmjs.com/package/elm
% brew install npm
% npm install -g elm
% npm install -g elm-css


# Emacs elm-mode
% use MELPA => M-x package-list-packages
% (require 'elm-mode)
% (setq auto-mode-alist (append '(("\\.elm$" . elm-mode)) auto-mode-alist))
% https://github.com/jcollard/elm-mode