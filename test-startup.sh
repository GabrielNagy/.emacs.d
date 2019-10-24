#!/bin/sh -xe
if [ -n "$TRAVIS" ]; then
    # Make it look like this is ~/.emacs.d (needed for Emacs 24.3, at least)
    export HOME=$PWD/..
    ln -s emacs.d ../.emacs.d
fi

if [ -n "$GITHUB_WORKSPACE" ]; then
    # Work around the github/checkout action by symlinking the checked out configuration
    ln -s $GITHUB_WORKSPACE $HOME/.emacs.d
fi
echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'
echo "Startup successful"
