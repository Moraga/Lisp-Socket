(require :sb-bsd-sockets)

(defpackage myserver
  (:use :cl :sb-bsd-sockets)
  (:export :main))

(in-package myserver)

(defparameter *port* 7000)

(defun make-echoer (stream id disconnector)
  (lambda (f)
    (declare (ignore f))
    (handler-case
     (let ((line (read stream)))
       (format t "~a ~a~%" id line)
       (cond ((member line '(nil -1 quit))
	      (funcall disconnector))
	     (t
	      (format stream "~a~%" (eval line))
	      (force-output stream))))
     (end-of-file ()
		  (funcall disconnector)))))

(defun make-disconnector (socket id)
  (lambda ()
    (let ((fd (socket-file-descriptor socket)))
      (format t "~a disconnected~%" id)
      (sb-impl::invalidate-descriptor fd)
      (socket-close socket))))

(defun serve (socket id)
  (let ((stream (socket-make-stream socket :output t :input t))
        (fd (socket-file-descriptor socket)))
    (sb-impl::add-fd-handler fd
                             :input
                             (make-echoer stream
                                          id
                                          (make-disconnector socket id)))))

(defun accept-connections (&optional (port *port*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
    (socket-bind socket #(127 0 0 1) port)
    (socket-listen socket 5)
    (sb-impl::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (f)
                               (declare (ignore f))
                               (incf counter)
                               (format t "~A connected~%" counter)
                               (serve (socket-accept socket) counter)))))

(defun main ()
  #+sb-thread
  (sb-thread:make-thread (lambda ()
			   (start)
			   (loop
                            (sb-impl::serve-all-events))))
  #-sb-thread
  (start)
  (loop
   (sleep 3600)))
