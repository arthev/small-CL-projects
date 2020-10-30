;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :tic-tac-tui
  :name "A tic-tac-toe game with TUI"
  :version "0.0"
  :author "arthev"
  :depends-on (pathnames personal-utilities)
  :components ((:file "packages-tui")
               (:file "ai")
               (:file "game")
               (:file "tui")))
