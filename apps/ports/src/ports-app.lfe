(defmodule ports-app
  (behaviour application)
  (export
   (start 2)
   (stop 1))
  (export
   (children 0)
   (info 0)
   (ports 0)
   (servers 0)
   (supervisor 0)))

(defun SUPERVISOR () 'ports-sup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_start-type _start-args)
  (logger:application 'ports-app)
  (logger:info "Starting application" '())
  (ports-sup:start_link))

(defun stop (_state)
  (ports-sup:stop)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun supervisor ()
  (erlang:whereis (SUPERVISOR)))

(defun children ()
  (supervisor:which_children (SUPERVISOR)))

(defun servers ()
  `(#(go-echo ,(go-echo-server:pid))
    #(lisp-echo ,(lisp-echo-server:pid))))

(defun ports ()
  `(#(go-echo ,(go-echo-server:port))
    #(lisp-echo ,(listp-echo-server:port))))

(defun info ()
  `(#(app ,(erlang:process_info (self) 'supervisor))
    #(supervisor ,(erlang:process_info (supervisor)))
    #(go-echo (#(server ,(erlang:process_info (go-echo-server:pid)))
               #(port ,(erlang:port_info (go-echo-server:port)))))
    #(lisp-echo (#(server ,(erlang:process_info (lisp-echo-server:pid)))
                 #(port ,(erlang:port_info (lisp-echo-server:port)))))))

