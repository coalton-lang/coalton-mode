This directory contins the source code for an Emacs mode that supports
working with Coalton code.

## Requirements

This mode requires Emacs version 29.1 and above, because it relies on
tree-sitter.

## Installation

In your emacs init file (probably ~/.emacs.d/init.el), 
add this directory to your load-path, and require the mode:

    ;; Coalton
    
    (add-to-list 'load-path "~/git/coalton-mode")
    (require 'coalton-mode)

## Usage

There is an example file in test/types.coal.

The first time you open a .coal file, Emacs will ask you to approve
the installation of a parser component:

    tree-sitter-coalton is not installed. Clone, build and install it?
    
(Answer 'yes')
