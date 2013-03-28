#!/bin/sh

home=~

 for file in `find /Users/abaranosky/work/architect/.`
 do
     if [[ $file =~ ^.*\.cljd?$ ]]; then
         emacs "$file" --batch --eval="(progn (load \"${home}/.emacs.d/elpa/clojure-mode-2.0.0/clojure-mode\") (require 'clojure-mode) (clojure-mode) (indent-region (point-min) (point-max)) (save-buffer))"
     fi
 done

echo "Slamhounding these files..."
echo `find /Users/abaranosky/work/architect/. -regex ".*\.clj"`
lein run -m slam.hound `find /Users/abaranosky/work/architect/. -regex ".*\.clj"`

echo "Running Kibit..."
lein2 kibit

echo "Running Eastwood"
lein eastwood

