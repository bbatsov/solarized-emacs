#!/bin/sh

exec emacs -Q --no-site  -l minimal-init/init.el minimal-init/test-files/ minimal-init/init.el solarized.el
