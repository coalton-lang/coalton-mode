This is an Emacs mode that supports working with Coalton. Most of the
logic is implemented as an LSP server.

## Requirements

This mode has been developed using `eglot`, which is included with
Emacs since version 29. The mode can be installed with
`M-x package-install RET eglot RET` in Emacs 26.3 and later.

In addition to Coalton, the LSP server process depends on the
following Lisp libraries:

- alexandria
- bordeaux-threads
- com.inuoe.jzon
- usocket
- trivial-utf-8

## Current Status

Supported LSP features:

- diagnostics: whenever a .coal file is saved, the contents are
  compiled, and diagnostics are published to the client.

## Usage

In the Emacs init file (e.g. `~/.emacs.d/init.el`), add this
directory to the load-path, and require the mode:

    ;; Coalton
    
    (add-to-list 'load-path "~/git/coalton-mode")
    (require 'coalton-mode)

Start the server in slime with:

    SLIME 2.30.git
    CL-USER> (asdf:load-system "coalton-lsp")
    ;; COALTON starting in development mode
    ;; COALTON starting with specializations enabled
    ;; COALTON starting with heuristic inlining disabled
    ;; COALTON will emit type annotations
    T
    CL-USER> (coalton-lsp::start-server)
    ;; :INFO coalton-mode: start #<SERVER 127.0.0.1:7887 {700D0A4333}>
    #<COALTON-MODE::SERVER 127.0.0.1:7887 {700D0A4333}>

In Emacs, open a .coal file (e.g. resources/fibonacci.coal), and
enable eglot mode:

    M-x eglot
