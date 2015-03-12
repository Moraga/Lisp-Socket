### Compile

sbcl --load socket.lisp

* (save-lisp-and-die "myserver" :toplevel #'myserver:main :executable t)