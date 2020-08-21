(defmodule ports.app
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

(defun SUPERVISOR () 'ports.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_start-type _start-args)
  (logger:set_application_level 'ports 'all)
  (logger:info "Starting application" '())
  (ports.supervisor:start_link))

(defun stop (_state)
  (ports.supervisor:stop)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supervisor ()
  (erlang:whereis (SUPERVISOR)))

(defun children ()
  (supervisor:which_children (SUPERVISOR)))

(defun servers ()
  `(#(go ,(ports.go.server:pid))
    #(lisp ,(ports.lisp.server:pid))))

(defun ports ()
  `(#(go ,(ports.go.server:port))
    #(lisp ,(ports.lisp.server:port))))

(defun info ()
  `(#(app ,(erlang:process_info (self)))
    #(supervisor ,(erlang:process_info (supervisor)))
    #(go (#(server ,(erlang:process_info (ports.go.server:pid)))
          #(port ,(erlang:port_info (ports.go.server:port)))))
    #(lisp (#(server ,(erlang:process_info (ports.lisp.server:pid)))
            #(port ,(erlang:port_info (ports.lisp.server:port)))))))
