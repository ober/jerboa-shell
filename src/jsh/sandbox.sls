#!chezscheme
(library (jsh sandbox)
  (export *current-jsh-env* run-script run-cmd jsh-sandbox-run
          parse-sb-args sb-parsed-script sb-parsed-cmd sb-parsed-opts)
  (import (chezscheme) (jsh script))
  
  (define *current-jsh-env* (make-parameter #f))
  
  (define (run-script filename . args)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-script filename args env)
        (error 'run-script "shell not initialized"))))
  
  (define (run-cmd cmd)
    (let ((env (*current-jsh-env*)))
      (if env
        (execute-string cmd env)
        (error 'run-cmd "shell not initialized"))))
  
  (define (jsh-sandbox-run opts thunk)
    (display "Warning: sandbox not available in static binary\n")
    (thunk))
  
  (define (parse-sb-args args) #f)
  (define (sb-parsed-script x) #f)
  (define (sb-parsed-cmd x) #f)
  (define (sb-parsed-opts x) '()))
