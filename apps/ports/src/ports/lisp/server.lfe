(defmodule ports.lisp.server
  (behaviour gen_server)
  (export
   (start_link 0)
   (stop 0))
  (export
   (code_change 3)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (init 1)
   (terminate 2))
  (export
   (pid 0)
   (port 0)
   (port-info 0)
   (send 1)))

(defun SERVER () (MODULE))
(defun DELIMITER () '(10))
(defun LISP-COMMAND () "sbcl")
(defun LISP-SERVER () "cl-port-examples/echo/src/main.lisp")
(defun LISP-TIMEOUT () 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stop ()
  (gen_server:call (MODULE) 'stop))

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER)) (MODULE) '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_args)
  (erlang:process_flag 'trap_exit 'true)
  `#(ok ,(create-port)))

(defun handle_call
  (('stop _from port)
   (stop-port port)
   `#(stop normal ok ,port))
  (('port _from port)
   `#(reply ,port ,port))
  ((msg from port)
   (let ((msg-bin (erlang:term_to_binary msg)))
     (logger:debug "Sending data: ~p" `(,msg-bin))
     (! port `#(,(self) #(command (,msg-bin ,(DELIMITER)))))
     (let ((data (ports.util:receive-line port (LISP-TIMEOUT))))
       (logger:debug "Got data: ~p" `(,data))
       (case data
         (#b()
           (logger:error "Got empty data from ~p; continuing ..." `(,from))
           `#(reply 'nodata ,port))
         (_
           (logger:debug "Data: ~p" `(,data))
           `#(reply ,(erlang:binary_to_term data '(safe)) ,port)))))))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_info
  ((`#(EXIT ,_from normal) port)
   (logger:info "The Lisp echo server is exiting (normal).")
   `#(noreply ,port))
  ((`#(EXIT ,_from shutdown) port)
   (logger:info "The Lisp echo server is exiting (shutdown).")
   `#(noreply ,port))
  ((`#(EXIT ,from ,reason) port)
   (logger:error "Lisp echo process ~p exited! (Reason: ~p)" `(,from ,reason))
   `#(noreply ,port))
  ((msg port)
   (logger:debug "The Lisp echo server is handling info of type: ~p." `(,msg))
   `#(noreply ,port)))

(defun terminate
  ((normal _port)
   (logger:info "The Lisp echo server is terminating."))
  ((shutdown _port)
   (logger:info "The supervisor is shutting down the Lisp echo server."))
  ((reason _port)
   (logger:info "The Lisp echo server is terminating for reason: ~p." `(,reason))))

(defun code_change (_old-version port _extra)
  `#(ok ,port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (MODULE)))

(defun port ()
  (gen_server:call (MODULE) 'port))

(defun port-info ()
  (erlang:port_info (port)))

(defun send (msg)
  (gen_server:call (MODULE) msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Internal Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-port ()
  (ports.util:create-port
    (LISP-COMMAND)
    (++ "--script " (filename:join (ports.util:priv-dir) (LISP-SERVER)))))

(defun stop-port (port)
    (! port `#(,(self) 'close)))
