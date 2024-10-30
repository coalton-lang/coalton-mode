;;;; Definition of the messages used by LSP using functions defined
;;;; by lib/message.lisp.
;;;;
;;;; See the file resources/spec.ts for the original TypeScript
;;;; definitions. The small message-definition language here is
;;;; intended to support idiomatic Lisp access to message contents,
;;;; (lisp-cased keyword keys, alists for maps, lists for sequences)
;;;; as well as reasonably strict validation of message completeness
;;;; and correctness when encoded as spec-compliant JSON.

(in-package #:coalton-lsp)

;;; Atom types: values that aren't encoded as maps or arrays.

;;; Type 't' indicates that the type of the value is determined by
;;; some other structural feature, such as the value of 'method' at
;;; the top of an RPC request or notification.

(define-atom t)

(define-atom string)

(deftype uri () 'string)

(define-atom uri)

(define-atom boolean)

(define-atom integer)

(deftype uinteger () '(integer 0 #.(1- (expt 2 31))))

(define-atom uinteger)

;;; Common structural types

(define-message position ()
  (:line uinteger)
  (:character uinteger))

(define-message range ()
  (:start position)
  (:end position))

(define-message location ()
  (:uri uri)
  (:range range))

(define-union token
    (integer string))

(define-union progress-token
    (integer string))

(define-message work-done-progress-params ()
  (:work-done-token token))

;;; Errors

(define-enum error-code ()
  (:unknown-error-code -32001)
  (:server-not-initialized -32002)
  (:invalid-request -32600)
  (:method-not-found -32601)
  (:invalid-params -32602)
  (:internal-error -32603)
  (:parse-error -32700)
  (:request-cancelled -32800)
  (:content-modified -32801)
  (:server-cancelled -32802)
  (:request-failed -32803))

(define-message response-error ()
  (:code error-code)
  (:message string)
  (:data t))

;;; Messages

(define-message message ()
  (:jsonrpc string))

(define-message notification-message (message)
  (:method string)
  (:params t))

(define-message request-message (message)
  (:id integer)
  (:method string)
  (:params t))

(define-message response-message (message)
  (:id integer)
  (:result (t :optional t))
  (:error (response-error :optional t)))

(define-enum position-encoding-kind ()
  (:utf8 "utf-8")
  (:utf16 "utf-16")
  (:utf32 "utf-32"))

(define-enum text-document-sync-kind ()
  (:none 0)
  (:full 1)
  (:incremental 2))

(define-message text-document-sync-options ()
  (:open-close boolean)
  (:change (text-document-sync-kind :optional t)))

(define-message server-info ()
  (:name string)
  (:version (string :optional t)))

(define-enum resource-operation-kind ()
  (:create "create")
  (:rename "rename")
  (:delete "delete"))

(define-enum failure-handling-kind ()
  (:abort "abort")
  (:transactional "transactional")
  (:undo "undo")
  (:text-only-transactional "textOnlyTransactional"))

(define-message workspace-edit-client-capabilities ()
  (:document-changes boolean)
  (:resource-operations (resource-operation-kind :vector t))
  (:failure-handling failure-handling-kind)
  (:normalizes-line-endings boolean)
  (:change-annotation-support boolean))

(define-message did-change-configuration-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message did-change-watched-files-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:relative-pattern-support (boolean :optional t)))

(define-enum symbol-kind ()
  (:file 1)
  (:module 2)
  (:namespace 3)
  (:package 4)
  (:class 5)
  (:method 6)
  (:property 7)
  (:field 8)
  (:constructor 9)
  (:enum 10)
  (:interface 11)
  (:function 12)
  (:variable 13)
  (:constant 14)
  (:string 15)
  (:number 16)
  (:boolean 17)
  (:array 18)
  (:object 19)
  (:key 20)
  (:null 21)
  (:enum-member 22)
  (:struct 23)
  (:event 24)
  (:operator 25)
  (:type-parameter 26))

(define-message symbol-kind-value-set ()
  (:value-set (symbol-kind :vector t)))

(define-enum symbol-tag ()
  (:deprecated 1))

(define-message symbol-tag-value-set ()
  (:value-set (symbol-tag :vector t)))

(define-message resolve-support-properties ()
  (:properties (string :vector t)))

(define-message workspace-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:resolve-support (resolve-support-properties :optional t)))

(define-message execute-command-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message semantic-tokens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message code-lens-workspace-client-capabilities ()
  (:refresh-support (boolean :optional t)))

(define-message workspace-client-capabilities ()
  (:apply-edit (boolean :optional t))
  (:workspace-edit (workspace-edit-client-capabilities :optional t))
  (:did-change-configuration (did-change-configuration-client-capabilities :optional t))
  (:did-change-watched-files (did-change-watched-files-client-capabilities :optional t))
  (:symbol (workspace-symbol-client-capabilities :optional t))
  (:execute-command (execute-command-client-capabilities :optional t))
  (:workspace-folders (boolean :optional t))
  (:configuration (boolean :optional t))
  (:semantic-tokens (semantic-tokens-workspace-client-capabilities :optional t))
  (:code-lens (code-lens-workspace-client-capabilities :optional t))
  #++ (:file-operations (file-operations-client-capabilities :optional t))
  #++ (:inline-value (inline-value-workspace-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-workspace-client-capabilities :optional t))
  #++ (:diagnostics (diagnostic-workspace-client-capabilities :optional t)))

(define-message text-document-sync-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:will-save (boolean :optional t))
  (:will-save-wait-until (boolean :optional t))
  (:did-save (boolean :optional t)))

(define-message declaration-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))


(define-enum insert-text-mode ()
  (:as-is 1)
  (:adjust-indentation 2))

(define-enum completion-item-tag ()
  (:deprecated t))

(define-message tag-support-value-set ()
  (:value-set (completion-item-tag :vector t)))

(define-message insert-text-mode-value-set ()
  (:value-set (insert-text-mode :vector t)))

(define-message resolve-support-properties ()
  (:properties (string :vector t)))

(define-enum markup-kind ()
  (:plaintext "plaintext")
  (:markdown "markdown"))

(define-message completion-item-capabilities ()
  (:snippet-support (boolean :optional t))
  (:commit-characters-support (boolean :optional t))
  (:documentation-format (markup-kind :optional t :vector t))
  (:deprecated-support (boolean :optional t))
  (:preselect-support (boolean :optional t))
  (:tag-support (tag-support-value-set :optional t))
  (:insert-replace-support (boolean :optional t))
  (:resolve-support (resolve-support-properties :optional t))
  (:insert-text-mode-support (insert-text-mode-value-set :optional t))
  (:label-details-support (boolean :optional t)))

(define-message completion-list-capabilities ()
  (:item-defaults (string :optional t :vector t)))

(define-message completion-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:completion-item completion-item-capabilities)
  #++ (:completion-item-kind completion-item-kind-capabilities)
  (:context-support (boolean :optional t))
  (:insert-text-mode insert-text-mode)
  (:completion-list completion-list-capabilities))

(define-message hover-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:content-format (markup-kind :vector t :optional t)))

(define-message signature-help-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  #++ (:signature-information (signature-information-options :optional t))
  (:context-support (boolean :optional t)))

(define-message type-definition-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message implementation-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:link-support (boolean :optional t)))

(define-message reference-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-highlight-client-capabilities ()
  (:dynamic-registration (boolean :optional t)))

(define-message document-symbol-client-capabilities ()
  (:dynamic-registration (boolean :optional t))
  (:symbol-kind (symbol-kind-value-set :optional t))
  (:hierarchical-document-symbol-support (boolean :optional t))
  (:tag-support (symbol-tag-value-set :optional t))
  (:label-support (boolean :optional t)))

(define-message text-document-client-capabilities ()
  (:synchronization (text-document-sync-client-capabilities :optional t))
  (:completion (completion-client-capabilities :optional t))
  (:hover (hover-client-capabilities :optional t))
  (:signature-help (signature-help-client-capabilities :optional t))
  (:declaration (declaration-client-capabilities :optional t))
  (:definition (definition-client-capabilities :optional t))
  (:type-definition (type-definition-client-capabilities :optional t))
  (:implementation (implementation-client-capabilities :optional t))
  (:references (reference-client-capabilities :optional t))
  (:document-highlight (document-highlight-client-capabilities :optional t))
  (:document-symbol (document-symbol-client-capabilities :optional t))
  #++ (:code-action (code-action-client-capabilities :optional t))
  #++ (:code-lens (code-lens-client-capabilities :optional t))
  #++ (:document-link (document-link-client-capabilities :optional t))
  #++ (:color-provider (document-color-client-capabilities :optional t))
  #++ (:formatting (document-formatting-client-capabilities :optional t))
  #++ (:range-formatting (document-range-formatting-client-capabilities :optional t))
  #++ (:on-type-formatting (document-on-type-formatting-client-capabilities :optional t))
  #++ (:rename (rename-client-capabilities :optional t))
  #++ (:publish-diagnostics (publish-diagnostics-client-capabilities :optional t))
  #++ (:folding-range (folding-range-client-capabilities :optional t))
  #++ (:selection-range (selection-range-client-capabilities :optional t))
  #++ (:linked-editing-range (linked-editing-range-client-capabilities :optional t))
  #++ (:call-hierarchy (call-hierarchy-client-capabilities :optional t))
  #++ (:semantic-tokens (semantic-tokens-client-capabilities :optional t))
  #++ (:moniker (moniker-client-capabilities :optional t))
  #++ (:type-hierarchy (type-hierarchy-client-capabilities :optional t))
  #++ (:inline-value (inline-value-client-capabilities :optional t))
  #++ (:inlay-hint (inlay-hint-client-capabilities :optional t))
  #++ (:diagnostic (diagnostic-client-capabilities :optional t)))

(define-message client-capabilities ()
  (:workspace (workspace-client-capabilities :optional t))
  (:text-document (text-document-client-capabilities :optional t))
  #++ (:notebook-document (notebook-document-client-capabilities :optional t))
  #++ (:window (window-client-capabilities :optional t))
  #++ (:general (general-client-capabilities :optional t))
  #++ (:experimental lsp-any))

(define-message client-info ()
  (:name string)
  (:version (string :optional t)))

(define-message workspace-folder ()
  (:uri uri)
  (:name string))

(define-enum trace-value ()
  (:off "off")
  (:messages "messages")
  (:verbose "verbose"))

;;; Session initialization
;;;
;;; The first message sent by the client is 'initialize', containing
;;; 'initialize-params' in the :params field, describing client
;;; capabilities. The server replies with 'initialized', and describes
;;; its capabilities.

(define-message initialize-params (work-done-progress-params)
  (:process-id (integer :optional t))
  (:client-info (client-info :optional t))
  (:locale (string :optional t))
  (:root-path (string :optional t))
  (:root-uri (uri :optional t))
  (:initialization-options t)
  (:capabilities client-capabilities)
  (:trace (trace-value :optional t))
  (:workspace-folders (workspace-folder :vector t :optional t)))

(define-message completion-item-options ()
  (:label-details-support (boolean :optional t)))

(define-message work-done-progress-options ()
  (:work-done-progress (boolean :optional t)))

(define-message declaration-options (work-done-progress-options))

(define-message definition-options (work-done-progress-options))

(define-message document-link-options (work-done-progress-options)
  (:resolve-provider (boolean :optional t)))

(define-message document-formatting-options (work-done-progress-options))

(define-message document-symbol-options (work-done-progress-options)
  (:label (string :optional t)))

(define-message completion-options (work-done-progress-options)
  (:trigger-characters (string :optional t :vector t))
  (:all-commit-characters (string :optional t :vector t))
  (:resolve-provider (boolean :optional t))
  (:completion-item (completion-item-options :optional t)))

(define-message document-filter ()
  (:language (string :optional t))
  (:scheme (string :optional t))
  (:pattern (string :optional t)))

(define-message text-document-registration-options ()
  (:document-selector (document-filter :vector t :optional t)))

(define-message empty ())

(define-message delta ()
  (:delta (boolean :optional t)))

(define-union range-option (boolean empty))

(define-union full-option (boolean delta))

(define-message semantic-tokens-legend ()
  (:token-types (string :vector t))
  (:token-modifiers (string :vector t)))

(define-message semantic-tokens-options (work-done-progress-options)
  (:legend semantic-tokens-legend)
  (:range (range-option :optional t))
  (:full (full-option :optional t)))

(define-message static-registration-options ()
  (:id (string :optional t)))

(define-message semantic-tokens-registration-options
    (text-document-registration-options
     semantic-tokens-options
     static-registration-options))

(define-union document-symbol-provider-options
  (boolean document-symbol-options))

(define-message server-capabilities ()
  (:position-encoding (position-encoding-kind :optional t))
  (:text-document-sync (text-document-sync-options :optional t))
  #++ (:notebook-document-sync (or notebook-document-sync-options notebook-document-sync-registration-options))
  (:completion-provider (completion-options :optional t))
  #++ (:hover-provider (or boolean hover-options))
  #++ (:signature-help-provider (signature-help-options :optional t))
  #++ (:declaration-provider (declaration-options declaration-registration-options))
  (:definition-provider (definition-options :optional t))
  #++ (:type-definition-provider (or boolean type-definition-options type-definition-registration-options))
  #++ (:implementation-provider (or boolean implementation-options implementation-registration-options))
  #++ (:references-provider (or boolean reference-options))
  #++ (:document-highlight-provider (or boolean document-highlight-options))
  (:document-symbol-provider (document-symbol-provider-options :optional t))
  #++ (:code-action-provider (or boolean code-action-options))
  #++ (:code-lens-provider (code-lens-options :optional t))
  (:document-link-provider (document-link-options :optional t))
  #++ (:color-provider (or boolean document-color-options document-color-registration-options))
  (:document-formatting-provider (document-formatting-options :optional t))
  #++ (:document-range-formatting-provider (or boolean document-range-formatting-options))
  #++ (:document-on-type-formatting-provider (document-on-type-formatting-options :optional t))
  #++ (:rename-provider (or boolean rename-options))
  #++ (:folding-range-provider (or boolean folding-range-options folding-range-registration-options))
  #++ (:execute-command-provider (execute-command-options :optional t))
  #++ (:selection-range-provider (or boolean selection-range-options selection-range-registration-options))
  #++ (:linked-editing-range-provider (or boolean linked-editing-range-options linked-editing-range-registration-options))
  #++ (:call-hierarchy-provider (or boolean call-hierarchy-options call-hierarchy-registration-options))
  (:semantic-tokens-provider semantic-tokens-registration-options)
  #++ (:moniker-provider (or boolean moniker-options moniker-registration-options))
  #++ (:type-hierarchy-provider (or boolean type-hierarchy-options type-hierarchy-registration-options))
  #++ (:inline-value-provider (or boolean inline-value-options inline-value-registration-options))
  #++ (:inlay-hint-provider (or boolean inlay-hint-options inlay-hint-registration-options))
  #++ (:diagnostic-provider (or diagnostic-options diagnostic-registration-options))
  #++ (:workspace-symbol-provider (boolean workspace-symbol-options))
  #++ (:workspace (workspace-server-capabilities :optional t))
  (:experimental (t :optional t)))

(define-message initialize-result ()
  (:capabilities server-capabilities)
  (:server-info server-info))

(defun handle-initialize (session params)
  (initializing-session session (message-value params))
  (let ((result (make-message 'initialize-result)))
    (set-field result (list :server-info :name) "Coalton")
    (set-field result (list :capabilities :text-document-sync :open-close) t)
    (set-field result (list :capabilities :text-document-sync :change) :full)
    (set-field result (list :capabilities :definition-provider :work-done-progress) t)
    (set-field result (list :capabilities :document-formatting-provider :work-done-progress) t)
    (set-field result (list :capabilities :document-symbol-provider) t)
    (set-field result (list :capabilities :semantic-tokens-provider :legend :token-types)
               '("namespace" "type" "function" "macro" "keyword" "class" "variable" "method"
                 "event" "interface"))
    (set-field result (list :capabilities :semantic-tokens-provider :legend :token-modifiers)
               '("definition" "defaultLibrary" "implementation"))
    (set-field result (list :capabilities :semantic-tokens-provider :range) t)
    (set-field result (list :capabilities :semantic-tokens-provider :full) t)
    (set-field result (list :capabilities :position-encoding)
               (position-encoding session))
    result))

(define-handler "initialize"
  initialize-params
  handle-initialize)

(defun handle-shutdown (session params)
  (declare (ignore params))
  (shutdown-session session)
  (make-message 'empty))

(define-handler "shutdown"
  empty
  handle-shutdown)

(define-message initialized-params ())

(defun handle-initialized (session params)
  (declare (ignore params))
  (initialized-session session))

(define-handler "initialized"
  initialized-params
  handle-initialized)

(define-message did-change-configuration-params ()
  (:settings t))

(defun handle-did-change-configuration (session params)
  (update-configuration session (get-field params :settings)))

(define-handler "workspace/didChangeConfiguration"
  did-change-configuration-params
  handle-did-change-configuration)

(define-message text-document-item ()
  (:uri uri)
  (:language-id string)
  (:version integer)
  (:text string))

;; notification: textDocument/didOpen

(define-message did-open-text-document-params ()
  (:text-document text-document-item))

(defun handle-text-document-did-open (session params)
  ;; (/debug "textDocument/didOpen = ~a" (get-field params (list :text-document :uri)))
  (open-document session (get-field params :text-document)))

(define-handler "textDocument/didOpen"
  did-open-text-document-params
  handle-text-document-did-open)

;; notification: textDocument/didChange

(define-message did-change-text-document-params ()
  (:text-document text-document-item))  ; FIXME this is wrong: use versioned version!

(defun handle-text-document-did-change (session params)
  ;; (/debug "textDocument/didChange = ~a" (get-field params (list :text-document :uri)))
  (change-document session (get-field params :text-document)))

(define-handler "textDocument/didChange"
  did-change-text-document-params
  handle-text-document-did-change)

;; end notification handler

(define-enum diagnostic-severity ()
  (:error 1)
  (:warning 2)
  (:information 3)
  (:hint 4))

(define-enum diagnostic-tag ()
  (:unnecessary 1)
  (:deprecated 2))

(define-message diagnostic-related-information ()
  (:location location)
  (:message string))

(define-message diagnostic ()
  (:range range)
  (:severity (diagnostic-severity :optional t))
  (:code (token :optional t))
  (:source (string :optional t))
  (:message string)
  (:tags (diagnostic-tag :vector t :optional t))
  (:related-information (diagnostic-related-information :vector t :optional t))
  (:data (t :optional t)))

;; notification: textDocument/publishDiagnostics

(define-message text-document-publish-diagnostics-params ()
  (:uri uri)
  (:version (integer :optional t))
  (:diagnostics (diagnostic :vector t)))

(defun handle-text-document-publish-diagnostics (session params)
  (declare (ignorable session params)))

(define-handler "textDocument/publishDiagnostics"
  text-document-publish-diagnostics-params
  handle-text-document-publish-diagnostics)

;; request: textDocument/semanticTokens/range

(defun session-semantic-tokens-range (session message)
  (declare (ignore session message))
  nil)

(define-message semantic-tokens ()
  (:data (uinteger :vector t)))

(defun handle-text-document-semantic-tokens-range (session params)
  (session-semantic-tokens-range session (message-value params))
  (let ((result (make-message 'semantic-tokens)))
    (set-field result (list :data)
               (list 0 9 9 0 0))
    result))

(define-message partial-result-params ()
  (:partial-result-token (progress-token :optional t)))

(define-message text-document-identifier ()
  (:uri uri))

(define-message semantic-tokens-range-params
    (work-done-progress-params
     partial-result-params)
  (:text-document text-document-identifier)
  (:range range))

(define-handler "textDocument/semanticTokens/range"
  semantic-tokens-range-params
  handle-text-document-semantic-tokens-range)
