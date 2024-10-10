// Language Server Protocol 3.17

type integer = number;

type uinteger = number;

type decimal = number;

type LSPAny = LSPObject | LSPArray | string | integer | uinteger |
    decimal | boolean | null;

type LSPObject = { [key: string]: LSPAny };

type LSPArray = LSPAny[];

interface Message {
    jsonrpc: string;
}

interface RequestMessage extends Message {
    id: integer | string;
    method: string;
    params?: array | object;
}

interface ResponseMessage extends Message {
    id: integer | string | null;
    result?: LSPAny;
    error?: ResponseError;
}

interface ResponseError {
    code: integer;
    message: string;
    data?: LSPAny;
}

namespace ErrorCodes {
    ParseError: integer = -32700;
    InvalidRequest: integer = -32600;
    MethodNotFound: integer = -32601;
    InvalidParams: integer = -32602;
    InternalError: integer = -32603;
    jsonrpcReservedErrorRangeStart: integer = -32099;
    serverErrorStart: integer = jsonrpcReservedErrorRangeStart;
    ServerNotInitialized: integer = -32002;
    UnknownErrorCode: integer = -32001;
    jsonrpcReservedErrorRangeEnd = -32000;
    serverErrorEnd: integer = jsonrpcReservedErrorRangeEnd;
    lspReservedErrorRangeStart: integer = -32899;
    RequestFailed: integer = -32803;
    ServerCancelled: integer = -32802;
    ContentModified: integer = -32801;
    RequestCancelled: integer = -32800;
    lspReservedErrorRangeEnd: integer = -32800;
}

interface NotificationMessage extends Message {
    method: string;
    params?: array | object;
}

interface CancelParams {
    id: integer | string;
}

type ProgressToken = integer | string;

interface ProgressParams& lt; T & gt; {
    token: ProgressToken;
    value: T;
}

interface HoverParams {
    textDocument: string; position: { line: uinteger; character: uinteger; };
}

interface HoverResult {
    value: string;
}

type DocumentUri = string;

type URI = string;

interface RegularExpressionsClientCapabilities {
    engine: string;
    version?: string;
}
EOL: string[] = ['\n', '\r\n', '\r'];

interface Position {
    line: uinteger;
    character: uinteger;
}

type PositionEncodingKind = string;

namespace PositionEncodingKind {
    UTF8: PositionEncodingKind = 'utf-8';
    UTF16: PositionEncodingKind = 'utf-16';
    UTF32: PositionEncodingKind = 'utf-32';
}

interface Range {
    start: Position;
    end: Position;
}

interface TextDocumentItem {
    uri: DocumentUri;
    languageId: string;
    version: integer;
    text: string;
}

interface TextDocumentIdentifier {
    uri: DocumentUri;
}

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: integer;
}

interface OptionalVersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    version: integer | null;
}

interface TextDocumentPositionParams {
    textDocument: TextDocumentIdentifier;
    position: Position;
}

interface DocumentFilter {
    language?: string;
    scheme?: string;
    pattern?: string;
}

type DocumentSelector = DocumentFilter[];

interface TextEdit {
    range: Range;
    newText: string;
}

interface ChangeAnnotation {
    label: string;
    needsConfirmation?: boolean;
    description?: string;
}

type ChangeAnnotationIdentifier = string;

interface AnnotatedTextEdit extends TextEdit {
    annotationId: ChangeAnnotationIdentifier;
}

interface TextDocumentEdit {
    textDocument: OptionalVersionedTextDocumentIdentifier;
    edits: (TextEdit | AnnotatedTextEdit)[];
}

interface Location {
    uri: DocumentUri;
    range: Range;
}

interface LocationLink {
    originSelectionRange?: Range;
    targetUri: DocumentUri;
    targetRange: Range;
    targetSelectionRange: Range;
}

interface Diagnostic {
    range: Range;
    severity?: DiagnosticSeverity;
    code?: integer | string;
    codeDescription?: CodeDescription;
    source?: string;
    message: string;
    tags?: DiagnosticTag[];
    relatedInformation?: DiagnosticRelatedInformation[];
    data?: LSPAny;
}

namespace DiagnosticSeverity {
    Error: 1 = 1;
    Warning: 2 = 2;
    Information: 3 = 3;
    Hint: 4 = 4;
}

type DiagnosticSeverity = 1 | 2 | 3 | 4;

namespace DiagnosticTag {
    Unnecessary: 1 = 1;
    Deprecated: 2 = 2;
}

type DiagnosticTag = 1 | 2;

interface DiagnosticRelatedInformation {
    location: Location;
    message: string;
}

interface CodeDescription {
    href: URI;
}

interface Command {
    title: string;
    command: string;
    arguments?: LSPAny[];
}

namespace MarkupKind {
    PlainText: 'plaintext' = 'plaintext';
    Markdown: 'markdown' = 'markdown';
}

type MarkupKind = 'plaintext' | 'markdown';

interface MarkupContent {
    kind: MarkupKind;
    value: string;
}

interface MarkdownClientCapabilities {
    parser: string;
    version?: string;
    allowedTags?: string[];
}

interface CreateFileOptions {
    overwrite?: boolean;
    ignoreIfExists?: boolean;
}

interface CreateFile {
    kind: 'create';
    uri: DocumentUri;
    options?: CreateFileOptions;
    annotationId?: ChangeAnnotationIdentifier;
}

interface RenameFileOptions {
    overwrite?: boolean;
    ignoreIfExists?: boolean;
}

interface RenameFile {
    kind: 'rename';
    oldUri: DocumentUri;
    newUri: DocumentUri;
    options?: RenameFileOptions;
    annotationId?: ChangeAnnotationIdentifier;
}

interface DeleteFileOptions {
    recursive?: boolean;
    ignoreIfNotExists?: boolean;
}

interface DeleteFile {
    kind: 'delete';
    uri: DocumentUri;
    options?: DeleteFileOptions;
    annotationId?: ChangeAnnotationIdentifier;
}

interface WorkspaceEdit {
    changes?: { [uri: DocumentUri]: TextEdit[]; };
    documentChanges?: (
        TextDocumentEdit[] |
        (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
    );
    changeAnnotations?: {
        [id: string /* ChangeAnnotationIdentifier */]: ChangeAnnotation;
    };
}

interface WorkspaceEditClientCapabilities {
    documentChanges?: boolean;
    resourceOperations?: ResourceOperationKind[];
    failureHandling?: FailureHandlingKind;
    normalizesLineEndings?: boolean;
    changeAnnotationSupport?: {
        groupsOnLabel?: boolean;
    };
}

type ResourceOperationKind = 'create' | 'rename' | 'delete';

namespace ResourceOperationKind {
    Create: ResourceOperationKind = 'create';
    Rename: ResourceOperationKind = 'rename';
    Delete: ResourceOperationKind = 'delete';
}

type FailureHandlingKind = 'abort' | 'transactional' | 'undo'
    | 'textOnlyTransactional';

namespace FailureHandlingKind {
    Abort: FailureHandlingKind = 'abort';
    Transactional: FailureHandlingKind = 'transactional';
    TextOnlyTransactional: FailureHandlingKind
        = 'textOnlyTransactional';
    Undo: FailureHandlingKind = 'undo';
}

interface WorkDoneProgressBegin {
    kind: 'begin';
    title: string;
    cancellable?: boolean;
    message?: string;
    percentage?: uinteger;
}

interface WorkDoneProgressReport {
    kind: 'report';
    cancellable?: boolean;
    message?: string;
    percentage?: uinteger;
}

interface WorkDoneProgressEnd {
    kind: 'end';
    message?: string;
}

interface WorkDoneProgressParams {
    workDoneToken?: ProgressToken;
}

interface WorkDoneProgressOptions {
    workDoneProgress?: boolean;
}

interface PartialResultParams {
    partialResultToken?: ProgressToken;
}

type TraceValue = 'off' | 'messages' | 'verbose';

interface InitializeParams extends WorkDoneProgressParams {
    processId: integer | null;
    clientInfo?: {
        name: string;
        version?: string;
    };
    locale?: string;
    rootPath?: string | null;
    rootUri: DocumentUri | null;
    initializationOptions?: LSPAny;
    capabilities: ClientCapabilities;
    trace?: TraceValue;
    workspaceFolders?: WorkspaceFolder[] | null;
}

interface TextDocumentClientCapabilities {
    synchronization?: TextDocumentSyncClientCapabilities;
    completion?: CompletionClientCapabilities;
    hover?: HoverClientCapabilities;
    signatureHelp?: SignatureHelpClientCapabilities;
    declaration?: DeclarationClientCapabilities;
    definition?: DefinitionClientCapabilities;
    typeDefinition?: TypeDefinitionClientCapabilities;
    implementation?: ImplementationClientCapabilities;
    references?: ReferenceClientCapabilities;
    documentHighlight?: DocumentHighlightClientCapabilities;
    documentSymbol?: DocumentSymbolClientCapabilities;
    codeAction?: CodeActionClientCapabilities;
    codeLens?: CodeLensClientCapabilities;
    documentLink?: DocumentLinkClientCapabilities;
    colorProvider?: DocumentColorClientCapabilities;
    formatting?: DocumentFormattingClientCapabilities;
    rangeFormatting?: DocumentRangeFormattingClientCapabilities;
    onTypeFormatting?: DocumentOnTypeFormattingClientCapabilities;
    rename?: RenameClientCapabilities;
    publishDiagnostics?: PublishDiagnosticsClientCapabilities;
    foldingRange?: FoldingRangeClientCapabilities;
    selectionRange?: SelectionRangeClientCapabilities;
    linkedEditingRange?: LinkedEditingRangeClientCapabilities;
    callHierarchy?: CallHierarchyClientCapabilities;
    semanticTokens?: SemanticTokensClientCapabilities;
    moniker?: MonikerClientCapabilities;
    typeHierarchy?: TypeHierarchyClientCapabilities;
    inlineValue?: InlineValueClientCapabilities;
    inlayHint?: InlayHintClientCapabilities;
    diagnostic?: DiagnosticClientCapabilities;
}

interface NotebookDocumentClientCapabilities {
    synchronization: NotebookDocumentSyncClientCapabilities;
}

interface ClientCapabilities {
    workspace?: {
        applyEdit?: boolean;
        workspaceEdit?: WorkspaceEditClientCapabilities;
        didChangeConfiguration?: DidChangeConfigurationClientCapabilities;
        didChangeWatchedFiles?: DidChangeWatchedFilesClientCapabilities;
        symbol?: WorkspaceSymbolClientCapabilities;
        executeCommand?: ExecuteCommandClientCapabilities;
        workspaceFolders?: boolean;
        configuration?: boolean;
        semanticTokens?: SemanticTokensWorkspaceClientCapabilities;
        codeLens?: CodeLensWorkspaceClientCapabilities;
        fileOperations?: {
            dynamicRegistration?: boolean;
            didCreate?: boolean;
            willCreate?: boolean;
            didRename?: boolean;
            willRename?: boolean;
            didDelete?: boolean;
            willDelete?: boolean;
        };
        inlineValue?: InlineValueWorkspaceClientCapabilities;
        inlayHint?: InlayHintWorkspaceClientCapabilities;
        diagnostics?: DiagnosticWorkspaceClientCapabilities;
    };
    textDocument?: TextDocumentClientCapabilities;
    notebookDocument?: NotebookDocumentClientCapabilities;
    window?: {
        workDoneProgress?: boolean;
        showMessage?: ShowMessageRequestClientCapabilities;
        showDocument?: ShowDocumentClientCapabilities;
    };
    general?: {
        staleRequestSupport?: {
            cancel: boolean;
            retryOnContentModified: string[];
        }
        regularExpressions?: RegularExpressionsClientCapabilities;
        markdown?: MarkdownClientCapabilities;
        positionEncodings?: PositionEncodingKind[];
    };
    experimental?: LSPAny;
}

interface InitializeResult {
    capabilities: ServerCapabilities;
    serverInfo?: {
        name: string;
        version?: string;
    };
}

namespace InitializeErrorCodes {
    unknownProtocolVersion: 1 = 1;
}

type InitializeErrorCodes = 1;

interface InitializeError {
    retry: boolean;
}

interface ServerCapabilities {
    positionEncoding?: PositionEncodingKind;
    textDocumentSync?: TextDocumentSyncOptions | TextDocumentSyncKind;
    notebookDocumentSync?: NotebookDocumentSyncOptions
    | NotebookDocumentSyncRegistrationOptions;
    completionProvider?: CompletionOptions;
    hoverProvider?: boolean | HoverOptions;
    signatureHelpProvider?: SignatureHelpOptions;
    declarationProvider?: boolean | DeclarationOptions
    | DeclarationRegistrationOptions;
    definitionProvider?: boolean | DefinitionOptions;
    typeDefinitionProvider?: boolean | TypeDefinitionOptions
    | TypeDefinitionRegistrationOptions;
    implementationProvider?: boolean | ImplementationOptions
    | ImplementationRegistrationOptions;
    referencesProvider?: boolean | ReferenceOptions;
    documentHighlightProvider?: boolean | DocumentHighlightOptions;
    documentSymbolProvider?: boolean | DocumentSymbolOptions;
    codeActionProvider?: boolean | CodeActionOptions;
    codeLensProvider?: CodeLensOptions;
    documentLinkProvider?: DocumentLinkOptions;
    colorProvider?: boolean | DocumentColorOptions
    | DocumentColorRegistrationOptions;
    documentFormattingProvider?: boolean | DocumentFormattingOptions;
    documentRangeFormattingProvider?: boolean | DocumentRangeFormattingOptions;
    documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
    renameProvider?: boolean | RenameOptions;
    foldingRangeProvider?: boolean | FoldingRangeOptions
    | FoldingRangeRegistrationOptions;
    executeCommandProvider?: ExecuteCommandOptions;
    selectionRangeProvider?: boolean | SelectionRangeOptions
    | SelectionRangeRegistrationOptions;
    linkedEditingRangeProvider?: boolean | LinkedEditingRangeOptions
    | LinkedEditingRangeRegistrationOptions;
    callHierarchyProvider?: boolean | CallHierarchyOptions
    | CallHierarchyRegistrationOptions;
    semanticTokensProvider?: SemanticTokensOptions
    | SemanticTokensRegistrationOptions;
    monikerProvider?: boolean | MonikerOptions | MonikerRegistrationOptions;
    typeHierarchyProvider?: boolean | TypeHierarchyOptions
    | TypeHierarchyRegistrationOptions;
    inlineValueProvider?: boolean | InlineValueOptions
    | InlineValueRegistrationOptions;
    inlayHintProvider?: boolean | InlayHintOptions
    | InlayHintRegistrationOptions;
    diagnosticProvider?: DiagnosticOptions | DiagnosticRegistrationOptions;
    workspaceSymbolProvider?: boolean | WorkspaceSymbolOptions;
    workspace?: {
        workspaceFolders?: WorkspaceFoldersServerCapabilities;
        fileOperations?: {
            didCreate?: FileOperationRegistrationOptions;
            willCreate?: FileOperationRegistrationOptions;
            didRename?: FileOperationRegistrationOptions;
            willRename?: FileOperationRegistrationOptions;
            didDelete?: FileOperationRegistrationOptions;
            willDelete?: FileOperationRegistrationOptions;
        };
    };
    experimental?: LSPAny;
}

interface InitializedParams {
}

interface Registration {
    id: string;
    method: string;
    registerOptions?: LSPAny;
}

interface RegistrationParams {
    registrations: Registration[];
}

interface StaticRegistrationOptions {
    id?: string;
}

interface TextDocumentRegistrationOptions {
    documentSelector: DocumentSelector | null;
}

interface Unregistration {
    id: string;
    method: string;
}

interface UnregistrationParams {
    unregisterations: Unregistration[];
}

interface SetTraceParams {
    value: TraceValue;
}

interface LogTraceParams {
    message: string;
    verbose?: string;
}

namespace TextDocumentSyncKind {
    None = 0;
    Full = 1;
    Incremental = 2;
}

type TextDocumentSyncKind = 0 | 1 | 2;

interface TextDocumentSyncOptions {
    openClose?: boolean;
    change?: TextDocumentSyncKind;
}

interface DidOpenTextDocumentParams {
    textDocument: TextDocumentItem;
}

interface TextDocumentChangeRegistrationOptions
    extends TextDocumentRegistrationOptions {
    syncKind: TextDocumentSyncKind;
}

interface DidChangeTextDocumentParams {
    textDocument: VersionedTextDocumentIdentifier;
    contentChanges: TextDocumentContentChangeEvent[];
}

type TextDocumentContentChangeEvent = {
    range: Range;
    rangeLength?: uinteger;
    text: string;
} | {
    text: string;
};

interface WillSaveTextDocumentParams {
    textDocument: TextDocumentIdentifier;
    reason: TextDocumentSaveReason;
}

namespace TextDocumentSaveReason {
    Manual = 1;
    AfterDelay = 2;
    FocusOut = 3;
}

type TextDocumentSaveReason = 1 | 2 | 3;

interface SaveOptions {
    includeText?: boolean;
}

interface TextDocumentSaveRegistrationOptions
    extends TextDocumentRegistrationOptions {
    includeText?: boolean;
}

interface DidSaveTextDocumentParams {
    textDocument: TextDocumentIdentifier;
    text?: string;
}

interface DidCloseTextDocumentParams {
    textDocument: TextDocumentIdentifier;
}

interface TextDocumentSyncClientCapabilities {
    dynamicRegistration?: boolean;
    willSave?: boolean;
    willSaveWaitUntil?: boolean;
    didSave?: boolean;
}

interface TextDocumentSyncOptions {
    openClose?: boolean;
    change?: TextDocumentSyncKind;
    willSave?: boolean;
    willSaveWaitUntil?: boolean;
    save?: boolean | SaveOptions;
}

interface NotebookDocument {
    uri: URI;
    notebookType: string;
    version: integer;
    metadata?: LSPObject;
    cells: NotebookCell[];
}

interface NotebookCell {
    kind: NotebookCellKind;
    document: DocumentUri;
    metadata?: LSPObject;
    executionSummary?: ExecutionSummary;
}

namespace NotebookCellKind {
    Markup: 1 = 1;
    Code: 2 = 2;
}

interface ExecutionSummary {
    executionOrder: uinteger;
    success?: boolean;
}

interface NotebookCellTextDocumentFilter {
    notebook: string | NotebookDocumentFilter;
    language?: string;
}

type NotebookDocumentFilter = {
    notebookType: string;
    scheme?: string;
    pattern?: string;
} | {
    notebookType?: string;
    scheme: string;
    pattern?: string;
} | {
    notebookType?: string;
    scheme?: string;
    pattern: string;
};

interface NotebookDocumentSyncClientCapabilities {
    dynamicRegistration?: boolean;
    executionSummarySupport?: boolean;
}

interface NotebookDocumentSyncOptions {
    notebookSelector: ({
        notebook: string | NotebookDocumentFilter;
        cells?: { language: string }[];
    } | {
        notebook?: string | NotebookDocumentFilter;
        cells: { language: string }[];
    })[];
    save?: boolean;
}

interface NotebookDocumentSyncRegistrationOptions extends
    NotebookDocumentSyncOptions, StaticRegistrationOptions {
}

interface DidOpenNotebookDocumentParams {
    notebookDocument: NotebookDocument;
    cellTextDocuments: TextDocumentItem[];
}

interface DidChangeNotebookDocumentParams {
    notebookDocument: VersionedNotebookDocumentIdentifier;
    change: NotebookDocumentChangeEvent;
}

interface VersionedNotebookDocumentIdentifier {
    version: integer;
    uri: URI;
}

interface NotebookDocumentChangeEvent {
    metadata?: LSPObject;
    cells?: {
        structure?: {
            array: NotebookCellArrayChange;
            didOpen?: TextDocumentItem[];
            didClose?: TextDocumentIdentifier[];
        };
        data?: NotebookCell[];
        textContent?: {
            document: VersionedTextDocumentIdentifier;
            changes: TextDocumentContentChangeEvent[];
        }[];
    };
}

interface NotebookCellArrayChange {
    start: uinteger;
    deleteCount: uinteger;
    cells?: NotebookCell[];
}

interface DidSaveNotebookDocumentParams {
    notebookDocument: NotebookDocumentIdentifier;
}

interface DidCloseNotebookDocumentParams {
    notebookDocument: NotebookDocumentIdentifier;
    cellTextDocuments: TextDocumentIdentifier[];
}

interface NotebookDocumentIdentifier {
    uri: URI;
}

interface DeclarationClientCapabilities {
    dynamicRegistration?: boolean;
    linkSupport?: boolean;
}

interface DeclarationOptions extends WorkDoneProgressOptions {
}

interface DeclarationRegistrationOptions extends DeclarationOptions,
    TextDocumentRegistrationOptions, StaticRegistrationOptions {
}

interface DeclarationParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

interface DefinitionClientCapabilities {
    dynamicRegistration?: boolean;
    linkSupport?: boolean;
}

interface DefinitionOptions extends WorkDoneProgressOptions {
}

interface DefinitionRegistrationOptions extends
    TextDocumentRegistrationOptions, DefinitionOptions {
}

interface DefinitionParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

interface TypeDefinitionClientCapabilities {
    dynamicRegistration?: boolean;
    linkSupport?: boolean;
}

interface TypeDefinitionOptions extends WorkDoneProgressOptions {
}

interface TypeDefinitionRegistrationOptions extends
    TextDocumentRegistrationOptions, TypeDefinitionOptions,
    StaticRegistrationOptions {
}

interface TypeDefinitionParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

interface ImplementationClientCapabilities {
    dynamicRegistration?: boolean;
    linkSupport?: boolean;
}

interface ImplementationOptions extends WorkDoneProgressOptions {
}

interface ImplementationRegistrationOptions extends
    TextDocumentRegistrationOptions, ImplementationOptions,
    StaticRegistrationOptions {
}

interface ImplementationParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

interface ReferenceClientCapabilities {
    dynamicRegistration?: boolean;
}

interface ReferenceOptions extends WorkDoneProgressOptions {
}

interface ReferenceRegistrationOptions extends
    TextDocumentRegistrationOptions, ReferenceOptions {
}

interface ReferenceParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
    context: ReferenceContext;
}

interface ReferenceContext {
    includeDeclaration: boolean;
}

interface CallHierarchyClientCapabilities {
    dynamicRegistration?: boolean;
}

interface CallHierarchyOptions extends WorkDoneProgressOptions {
}

interface CallHierarchyRegistrationOptions extends
    TextDocumentRegistrationOptions, CallHierarchyOptions,
    StaticRegistrationOptions {
}

interface CallHierarchyPrepareParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
}

interface CallHierarchyItem {
    name: string;
    kind: SymbolKind;
    tags?: SymbolTag[];
    detail?: string;
    uri: DocumentUri;
    range: Range;
    selectionRange: Range;
    data?: LSPAny;
}

interface CallHierarchyIncomingCallsParams extends
    WorkDoneProgressParams, PartialResultParams {
    item: CallHierarchyItem;
}

interface CallHierarchyIncomingCall {
    from: CallHierarchyItem;
    fromRanges: Range[];
}

interface CallHierarchyOutgoingCallsParams extends
    WorkDoneProgressParams, PartialResultParams {
    item: CallHierarchyItem;
}

interface CallHierarchyOutgoingCall {
    to: CallHierarchyItem;
    fromRanges: Range[];
}

type TypeHierarchyClientCapabilities = {
    dynamicRegistration?: boolean;
};

interface TypeHierarchyOptions extends WorkDoneProgressOptions {
}

interface TypeHierarchyRegistrationOptions extends
    TextDocumentRegistrationOptions, TypeHierarchyOptions,
    StaticRegistrationOptions {
}

interface TypeHierarchyPrepareParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
}

interface TypeHierarchyItem {
    name: string;
    kind: SymbolKind;
    tags?: SymbolTag[];
    detail?: string;
    uri: DocumentUri;
    range: Range;
    selectionRange: Range;
    data?: LSPAny;
}

interface TypeHierarchySupertypesParams extends
    WorkDoneProgressParams, PartialResultParams {
    item: TypeHierarchyItem;
}

interface TypeHierarchySubtypesParams extends
    WorkDoneProgressParams, PartialResultParams {
    item: TypeHierarchyItem;
}

interface DocumentHighlightClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DocumentHighlightOptions extends WorkDoneProgressOptions {
}

interface DocumentHighlightRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentHighlightOptions {
}

interface DocumentHighlightParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

interface DocumentHighlight {
    range: Range;
    kind?: DocumentHighlightKind;
}

namespace DocumentHighlightKind {
    Text = 1;
    Read = 2;
    Write = 3;
}

type DocumentHighlightKind = 1 | 2 | 3;

interface DocumentLinkClientCapabilities {
    dynamicRegistration?: boolean;
    tooltipSupport?: boolean;
}

interface DocumentLinkOptions extends WorkDoneProgressOptions {
    resolveProvider?: boolean;
}

interface DocumentLinkRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentLinkOptions {
}

interface DocumentLinkParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

interface DocumentLink {
    range: Range;
    target?: URI;
    tooltip?: string;
    data?: LSPAny;
}

interface HoverClientCapabilities {
    dynamicRegistration?: boolean;
    contentFormat?: MarkupKind[];
}

interface HoverOptions extends WorkDoneProgressOptions {
}

interface HoverRegistrationOptions
    extends TextDocumentRegistrationOptions, HoverOptions {
}

interface HoverParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
}

interface Hover {
    contents: MarkedString | MarkedString[] | MarkupContent;
    range?: Range;
}

type MarkedString = string | { language: string; value: string };

interface CodeLensClientCapabilities {
    dynamicRegistration?: boolean;
}

interface CodeLensOptions extends WorkDoneProgressOptions {
    resolveProvider?: boolean;
}

interface CodeLensRegistrationOptions extends
    TextDocumentRegistrationOptions, CodeLensOptions {
}

interface CodeLensParams extends WorkDoneProgressParams, PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

interface CodeLens {
    range: Range;
    command?: Command;
    data?: LSPAny;
}

interface CodeLensWorkspaceClientCapabilities {
    refreshSupport?: boolean;
}

interface FoldingRangeClientCapabilities {
    dynamicRegistration?: boolean;
    rangeLimit?: uinteger;
    lineFoldingOnly?: boolean;
    foldingRangeKind?: {
        valueSet?: FoldingRangeKind[];
    };
    foldingRange?: {
        collapsedText?: boolean;
    };
}

interface FoldingRangeOptions extends WorkDoneProgressOptions {
}

interface FoldingRangeRegistrationOptions extends
    TextDocumentRegistrationOptions, FoldingRangeOptions,
    StaticRegistrationOptions {
}

interface FoldingRangeParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

namespace FoldingRangeKind {
    Comment = 'comment';
    Imports = 'imports';
    Region = 'region';
}

type FoldingRangeKind = string;

interface FoldingRange {
    startLine: uinteger;
    startCharacter?: uinteger;
    endLine: uinteger;
    endCharacter?: uinteger;
    kind?: FoldingRangeKind;
    collapsedText?: string;
}

interface SelectionRangeClientCapabilities {
    dynamicRegistration?: boolean;
}

interface SelectionRangeOptions extends WorkDoneProgressOptions {
}

interface SelectionRangeRegistrationOptions extends
    SelectionRangeOptions, TextDocumentRegistrationOptions,
    StaticRegistrationOptions {
}

interface SelectionRangeParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    positions: Position[];
}

interface SelectionRange {
    range: Range;
    parent?: SelectionRange;
}

interface DocumentSymbolClientCapabilities {
    dynamicRegistration?: boolean;
    symbolKind?: {
        valueSet?: SymbolKind[];
    };
    hierarchicalDocumentSymbolSupport?: boolean;
    tagSupport?: {
        valueSet: SymbolTag[];
    };
    labelSupport?: boolean;
}

interface DocumentSymbolOptions extends WorkDoneProgressOptions {
    label?: string;
}

interface DocumentSymbolRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentSymbolOptions {
}

interface DocumentSymbolParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

namespace SymbolKind {
    File = 1;
    Module = 2;
    Namespace = 3;
    Package = 4;
    Class = 5;
    Method = 6;
    Property = 7;
    Field = 8;
    Constructor = 9;
    Enum = 10;
    Interface = 11;
    Function = 12;
    Variable = 13;
    Constant = 14;
    String = 15;
    Number = 16;
    Boolean = 17;
    Array = 18;
    Object = 19;
    Key = 20;
    Null = 21;
    EnumMember = 22;
    Struct = 23;
    Event = 24;
    Operator = 25;
    TypeParameter = 26;
}

type SymbolKind = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26;

namespace SymbolTag {
    Deprecated: 1 = 1;
}

type SymbolTag = 1;

interface DocumentSymbol {
    name: string;
    detail?: string;
    kind: SymbolKind;
    tags?: SymbolTag[];
    deprecated?: boolean;
    range: Range;
    selectionRange: Range;
    children?: DocumentSymbol[];
}

interface SymbolInformation {
    name: string;
    kind: SymbolKind;
    tags?: SymbolTag[];
    deprecated?: boolean;
    location: Location;
    containerName?: string;
}

enum SemanticTokenTypes {
    namespace = 'namespace',
    type = 'type',
    class = 'class',
    enum = 'enum',
    interface = 'interface',
    struct = 'struct',
    typeParameter = 'typeParameter',
    parameter = 'parameter',
    variable = 'variable',
    property = 'property',
    enumMember = 'enumMember',
    event = 'event',
    function = 'function',
    method = 'method',
    macro = 'macro',
    keyword = 'keyword',
    modifier = 'modifier',
    comment = 'comment',
    string = 'string',
    number = 'number',
    regexp = 'regexp',
    operator = 'operator',
    decorator = 'decorator'
}

enum SemanticTokenModifiers {
    declaration = 'declaration',
    definition = 'definition',
    readonly = 'readonly',
    static = 'static',
    deprecated = 'deprecated',
    abstract = 'abstract',
    async = 'async',
    modification = 'modification',
    documentation = 'documentation',
    defaultLibrary = 'defaultLibrary'
}

namespace TokenFormat {
    Relative: 'relative' = 'relative';
}

type TokenFormat = 'relative';

interface SemanticTokensLegend {
    tokenTypes: string[];
    tokenModifiers: string[];
}

interface SemanticTokensClientCapabilities {
    dynamicRegistration?: boolean;
    requests: {
        range?: boolean | {
        };
        full?: boolean | {
            delta?: boolean;
        };
    };
    tokenTypes: string[];
    tokenModifiers: string[];
    formats: TokenFormat[];
    overlappingTokenSupport?: boolean;
    multilineTokenSupport?: boolean;
    serverCancelSupport?: boolean;
    augmentsSyntaxTokens?: boolean;
}

interface SemanticTokensOptions extends WorkDoneProgressOptions {
    legend: SemanticTokensLegend;
    range?: boolean | {
    };
    full?: boolean | {
        delta?: boolean;
    };
}

interface SemanticTokensRegistrationOptions extends
    TextDocumentRegistrationOptions, SemanticTokensOptions,
    StaticRegistrationOptions {
}

interface SemanticTokensParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

interface SemanticTokens {
    resultId?: string;
    data: uinteger[];
}

interface SemanticTokensPartialResult {
    data: uinteger[];
}

interface SemanticTokensDeltaParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    previousResultId: string;
}

interface SemanticTokensDelta {
    readonly resultId?: string;
    edits: SemanticTokensEdit[];
}

interface SemanticTokensEdit {
    start: uinteger;
    deleteCount: uinteger;
    data?: uinteger[];
}

interface SemanticTokensDeltaPartialResult {
    edits: SemanticTokensEdit[];
}

interface SemanticTokensRangeParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
}

interface SemanticTokensWorkspaceClientCapabilities {
    refreshSupport?: boolean;
}

interface InlayHintClientCapabilities {
    dynamicRegistration?: boolean;
    resolveSupport?: {
        properties: string[];
    };
}

interface InlayHintOptions extends WorkDoneProgressOptions {
    resolveProvider?: boolean;
}

interface InlayHintRegistrationOptions extends InlayHintOptions,
    TextDocumentRegistrationOptions, StaticRegistrationOptions {
}

interface InlayHintParams extends WorkDoneProgressParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
}

interface InlayHint {
    position: Position;
    label: string | InlayHintLabelPart[];
    kind?: InlayHintKind;
    textEdits?: TextEdit[];
    tooltip?: string | MarkupContent;
    paddingLeft?: boolean;
    paddingRight?: boolean;
    data?: LSPAny;
}

interface InlayHintLabelPart {
    value: string;
    tooltip?: string | MarkupContent;
    location?: Location;
    command?: Command;
}

namespace InlayHintKind {
    Type = 1;
    Parameter = 2;
}

type InlayHintKind = 1 | 2;

interface InlayHintWorkspaceClientCapabilities {
    refreshSupport?: boolean;
}

interface InlineValueClientCapabilities {
    dynamicRegistration?: boolean;
}

interface InlineValueOptions extends WorkDoneProgressOptions {
}

interface InlineValueRegistrationOptions extends InlineValueOptions,
    TextDocumentRegistrationOptions, StaticRegistrationOptions {
}

interface InlineValueParams extends WorkDoneProgressParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    context: InlineValueContext;
}

interface InlineValueContext {
    frameId: integer;
    stoppedLocation: Range;
}

interface InlineValueText {
    range: Range;
    text: string;
}

interface InlineValueVariableLookup {
    range: Range;
    variableName?: string;
    caseSensitiveLookup: boolean;
}

interface InlineValueEvaluatableExpression {
    range: Range;
    expression?: string;
}

type InlineValue = InlineValueText | InlineValueVariableLookup
    | InlineValueEvaluatableExpression;

interface InlineValueWorkspaceClientCapabilities {
    refreshSupport?: boolean;
}

interface MonikerClientCapabilities {
    dynamicRegistration?: boolean;
}

interface MonikerOptions extends WorkDoneProgressOptions {
}

interface MonikerRegistrationOptions extends
    TextDocumentRegistrationOptions, MonikerOptions {
}

interface MonikerParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
}

enum UniquenessLevel {
    document = 'document',
    project = 'project',
    group = 'group',
    scheme = 'scheme',
    global = 'global'
}

enum MonikerKind {
    import = 'import',
    = 'export',
    local = 'local'
}

interface Moniker {
    scheme: string;
    identifier: string;
    unique: UniquenessLevel;
    kind?: MonikerKind;
}

interface CompletionClientCapabilities {
    dynamicRegistration?: boolean;
    completionItem?: {
        snippetSupport?: boolean;
        commitCharactersSupport?: boolean;
        documentationFormat?: MarkupKind[];
        deprecatedSupport?: boolean;
        preselectSupport?: boolean;
        tagSupport?: {
            valueSet: CompletionItemTag[];
        };
        insertReplaceSupport?: boolean;
        resolveSupport?: {
            properties: string[];
        };
        insertTextModeSupport?: {
            valueSet: InsertTextMode[];
        };
        labelDetailsSupport?: boolean;
    };
    completionItemKind?: {
        valueSet?: CompletionItemKind[];
    };
    contextSupport?: boolean;
    insertTextMode?: InsertTextMode;
    completionList?: {
        itemDefaults?: string[];
    }
}

interface CompletionOptions extends WorkDoneProgressOptions {
    triggerCharacters?: string[];
    allCommitCharacters?: string[];
    resolveProvider?: boolean;
    completionItem?: {
        labelDetailsSupport?: boolean;
    }
}

interface CompletionRegistrationOptions
    extends TextDocumentRegistrationOptions, CompletionOptions {
}

interface CompletionParams extends TextDocumentPositionParams,
    WorkDoneProgressParams, PartialResultParams {
    context?: CompletionContext;
}

namespace CompletionTriggerKind {
    Invoked: 1 = 1;
    TriggerCharacter: 2 = 2;
    TriggerForIncompleteCompletions: 3 = 3;
}

type CompletionTriggerKind = 1 | 2 | 3;

interface CompletionContext {
    triggerKind: CompletionTriggerKind;
    triggerCharacter?: string;
}

interface CompletionList {
    isIncomplete: boolean;
    itemDefaults?: {
        commitCharacters?: string[];
        editRange?: Range | {
            insert: Range;
            replace: Range;
        };
        insertTextFormat?: InsertTextFormat;
        insertTextMode?: InsertTextMode;
        data?: LSPAny;
    }
    items: CompletionItem[];
}

namespace InsertTextFormat {
    PlainText = 1;
    Snippet = 2;
}

type InsertTextFormat = 1 | 2;

namespace CompletionItemTag {
    Deprecated = 1;
}

type CompletionItemTag = 1;

interface InsertReplaceEdit {
    newText: string;
    insert: Range;
    replace: Range;
}

namespace InsertTextMode {
    asIs: 1 = 1;
    adjustIndentation: 2 = 2;
}

type InsertTextMode = 1 | 2;

interface CompletionItemLabelDetails {
    detail?: string;
    description?: string;
}

interface CompletionItem {
    label: string;
    labelDetails?: CompletionItemLabelDetails;
    kind?: CompletionItemKind;
    tags?: CompletionItemTag[];
    detail?: string;
    documentation?: string | MarkupContent;
    deprecated?: boolean;
    preselect?: boolean;
    sortText?: string;
    filterText?: string;
    insertText?: string;
    insertTextFormat?: InsertTextFormat;
    insertTextMode?: InsertTextMode;
    textEdit?: TextEdit | InsertReplaceEdit;
    textEditText?: string;
    additionalTextEdits?: TextEdit[];
    commitCharacters?: string[];
    command?: Command;
    data?: LSPAny;
}

namespace CompletionItemKind {
    Text = 1;
    Method = 2;
    Function = 3;
    Constructor = 4;
    Field = 5;
    Variable = 6;
    Class = 7;
    Interface = 8;
    Module = 9;
    Property = 10;
    Unit = 11;
    Value = 12;
    Enum = 13;
    Keyword = 14;
    Snippet = 15;
    Color = 16;
    File = 17;
    Reference = 18;
    Folder = 19;
    EnumMember = 20;
    Constant = 21;
    Struct = 22;
    Event = 23;
    Operator = 24;
    TypeParameter = 25;
}

type CompletionItemKind = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25;

interface PublishDiagnosticsClientCapabilities {
    relatedInformation?: boolean;
    tagSupport?: {
        valueSet: DiagnosticTag[];
    };
    versionSupport?: boolean;
    codeDescriptionSupport?: boolean;
    dataSupport?: boolean;
}

interface PublishDiagnosticsParams {
    uri: DocumentUri;
    version?: integer;
    diagnostics: Diagnostic[];
}

interface DiagnosticClientCapabilities {
    dynamicRegistration?: boolean;
    relatedDocumentSupport?: boolean;
}

interface DiagnosticOptions extends WorkDoneProgressOptions {
    identifier?: string;
    interFileDependencies: boolean;
    workspaceDiagnostics: boolean;
}

interface DiagnosticRegistrationOptions extends
    TextDocumentRegistrationOptions, DiagnosticOptions,
    StaticRegistrationOptions {
}

interface DocumentDiagnosticParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    identifier?: string;
    previousResultId?: string;
}

type DocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport
    | RelatedUnchangedDocumentDiagnosticReport;

namespace DocumentDiagnosticReportKind {
    Full = 'full';
    Unchanged = 'unchanged';
}

type DocumentDiagnosticReportKind = 'full' | 'unchanged';

interface FullDocumentDiagnosticReport {
    kind: DocumentDiagnosticReportKind.Full;
    resultId?: string;
    items: Diagnostic[];
}

interface UnchangedDocumentDiagnosticReport {
    kind: DocumentDiagnosticReportKind.Unchanged;
    resultId: string;
}

interface RelatedFullDocumentDiagnosticReport extends
    FullDocumentDiagnosticReport {
    relatedDocuments?: {
        [uri: string :
        FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
	};
}

interface RelatedUnchangedDocumentDiagnosticReport extends
    UnchangedDocumentDiagnosticReport {
    relatedDocuments?: {
        [uri: string :
        FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
	};
}

interface DocumentDiagnosticReportPartialResult {
    relatedDocuments: {
        [uri: string :
        FullDocumentDiagnosticReport | UnchangedDocumentDiagnosticReport;
	};
}

interface DiagnosticServerCancellationData {
    retriggerRequest: boolean;
}

interface WorkspaceDiagnosticParams extends WorkDoneProgressParams,
    PartialResultParams {
    identifier?: string;
    previousResultIds: PreviousResultId[];
}

interface PreviousResultId {
    uri: DocumentUri;
    value: string;
}

interface WorkspaceDiagnosticReport {
    items: WorkspaceDocumentDiagnosticReport[];
}

interface WorkspaceFullDocumentDiagnosticReport extends
    FullDocumentDiagnosticReport {
    uri: DocumentUri;
    version: integer | null;
}

interface WorkspaceUnchangedDocumentDiagnosticReport extends
    UnchangedDocumentDiagnosticReport {
    uri: DocumentUri;
    version: integer | null;
};

type WorkspaceDocumentDiagnosticReport =
    WorkspaceFullDocumentDiagnosticReport
    | WorkspaceUnchangedDocumentDiagnosticReport;

interface WorkspaceDiagnosticReportPartialResult {
    items: WorkspaceDocumentDiagnosticReport[];
}

interface DiagnosticWorkspaceClientCapabilities {
    refreshSupport?: boolean;
}

interface SignatureHelpClientCapabilities {
    dynamicRegistration?: boolean;
    signatureInformation?: {
        documentationFormat?: MarkupKind[];
        parameterInformation?: {
            labelOffsetSupport?: boolean;
        };
        activeParameterSupport?: boolean;
    };
    contextSupport?: boolean;
}

interface SignatureHelpOptions extends WorkDoneProgressOptions {
    triggerCharacters?: string[];
    retriggerCharacters?: string[];
}

interface SignatureHelpRegistrationOptions
    extends TextDocumentRegistrationOptions, SignatureHelpOptions {
}

interface SignatureHelpParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
    context?: SignatureHelpContext;
}

namespace SignatureHelpTriggerKind {
    Invoked: 1 = 1;
    TriggerCharacter: 2 = 2;
    ContentChange: 3 = 3;
}

type SignatureHelpTriggerKind = 1 | 2 | 3;

interface SignatureHelpContext {
    triggerKind: SignatureHelpTriggerKind;
    triggerCharacter?: string;
    isRetrigger: boolean;
    activeSignatureHelp?: SignatureHelp;
}

interface SignatureHelp {
    signatures: SignatureInformation[];
    activeSignature?: uinteger;
    activeParameter?: uinteger;
}

interface SignatureInformation {
    label: string;
    documentation?: string | MarkupContent;
    parameters?: ParameterInformation[];
    activeParameter?: uinteger;
}

interface ParameterInformation {
    label: string | [uinteger, uinteger];
    documentation?: string | MarkupContent;
}

interface CodeActionClientCapabilities {
    dynamicRegistration?: boolean;
    codeActionLiteralSupport?: {
        codeActionKind: {
            valueSet: CodeActionKind[];
        };
    };
    isPreferredSupport?: boolean;
    disabledSupport?: boolean;
    dataSupport?: boolean;
    resolveSupport?: {
        properties: string[];
    };
    honorsChangeAnnotations?: boolean;
}

interface CodeActionOptions extends WorkDoneProgressOptions {
    codeActionKinds?: CodeActionKind[];
    resolveProvider?: boolean;
}

interface CodeActionRegistrationOptions extends
    TextDocumentRegistrationOptions, CodeActionOptions {
}

interface CodeActionParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    context: CodeActionContext;
}

type CodeActionKind = string;

namespace CodeActionKind {
    Empty: CodeActionKind = '';
    QuickFix: CodeActionKind = 'quickfix';
    Refactor: CodeActionKind = 'refactor';
    RefactorExtract: CodeActionKind = 'refactor.extract';
    RefactorInline: CodeActionKind = 'refactor.inline';
    RefactorRewrite: CodeActionKind = 'refactor.rewrite';
    Source: CodeActionKind = 'source';
    SourceOrganizeImports: CodeActionKind =
        'source.organizeImports';
    SourceFixAll: CodeActionKind = 'source.fixAll';
}

interface CodeActionContext {
    diagnostics: Diagnostic[];
    only?: CodeActionKind[];
    triggerKind?: CodeActionTriggerKind;
}

namespace CodeActionTriggerKind {
    Invoked: 1 = 1;
    Automatic: 2 = 2;
}

type CodeActionTriggerKind = 1 | 2;

interface CodeAction {
    title: string;
    kind?: CodeActionKind;
    diagnostics?: Diagnostic[];
    isPreferred?: boolean;
    disabled?: {
        reason: string;
    };
    edit?: WorkspaceEdit;
    command?: Command;
    data?: LSPAny;
}

interface DocumentColorClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DocumentColorOptions extends WorkDoneProgressOptions {
}

interface DocumentColorRegistrationOptions extends
    TextDocumentRegistrationOptions, StaticRegistrationOptions,
    DocumentColorOptions {
}

interface DocumentColorParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
}

interface ColorInformation {
    range: Range;
    color: Color;
}

interface Color {
    readonly red: decimal;
    readonly green: decimal;
    readonly blue: decimal;
    readonly alpha: decimal;
}

interface ColorPresentationParams extends WorkDoneProgressParams,
    PartialResultParams {
    textDocument: TextDocumentIdentifier;
    color: Color;
    range: Range;
}

interface ColorPresentation {
    label: string;
    textEdit?: TextEdit;
    additionalTextEdits?: TextEdit[];
}

interface DocumentFormattingClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DocumentFormattingOptions extends WorkDoneProgressOptions {
}

interface DocumentFormattingRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentFormattingOptions {
}

interface DocumentFormattingParams extends WorkDoneProgressParams {
    textDocument: TextDocumentIdentifier;
    options: FormattingOptions;
}

interface FormattingOptions {
    tabSize: uinteger;
    insertSpaces: boolean;
    trimTrailingWhitespace?: boolean;
    insertFinalNewline?: boolean;
    trimFinalNewlines?: boolean;
    [key: string]: boolean | integer | string;
}

interface DocumentRangeFormattingClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DocumentRangeFormattingOptions extends
    WorkDoneProgressOptions {
}

interface DocumentRangeFormattingRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentRangeFormattingOptions {
}

interface DocumentRangeFormattingParams extends WorkDoneProgressParams {
    textDocument: TextDocumentIdentifier;
    range: Range;
    options: FormattingOptions;
}

interface DocumentOnTypeFormattingClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DocumentOnTypeFormattingOptions {
    firstTriggerCharacter: string;
    moreTriggerCharacter?: string[];
}

interface DocumentOnTypeFormattingRegistrationOptions extends
    TextDocumentRegistrationOptions, DocumentOnTypeFormattingOptions {
}

interface DocumentOnTypeFormattingParams {
    textDocument: TextDocumentIdentifier;
    position: Position;
    ch: string;
    options: FormattingOptions;
}

namespace PrepareSupportDefaultBehavior {
    Identifier: 1 = 1;
}

type PrepareSupportDefaultBehavior = 1;

interface RenameClientCapabilities {
    dynamicRegistration?: boolean;
    prepareSupport?: boolean;
    prepareSupportDefaultBehavior?: PrepareSupportDefaultBehavior;
    honorsChangeAnnotations?: boolean;
}

interface RenameOptions extends WorkDoneProgressOptions {
    prepareProvider?: boolean;
}

interface RenameRegistrationOptions extends
    TextDocumentRegistrationOptions, RenameOptions {
}

interface RenameParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
    newName: string;
}

interface PrepareRenameParams extends TextDocumentPositionParams, WorkDoneProgressParams {
}

interface LinkedEditingRangeClientCapabilities {
    dynamicRegistration?: boolean;
}

interface LinkedEditingRangeOptions extends WorkDoneProgressOptions {
}

interface LinkedEditingRangeRegistrationOptions extends
    TextDocumentRegistrationOptions, LinkedEditingRangeOptions,
    StaticRegistrationOptions {
}

interface LinkedEditingRangeParams extends TextDocumentPositionParams,
    WorkDoneProgressParams {
}

interface LinkedEditingRanges {
    ranges: Range[];
    wordPattern?: string;
}

interface WorkspaceSymbolClientCapabilities {
    dynamicRegistration?: boolean;
    symbolKind?: {
        valueSet?: SymbolKind[];
    };
    tagSupport?: {
        valueSet: SymbolTag[];
    };
    resolveSupport?: {
        properties: string[];
    };
}

interface WorkspaceSymbolOptions extends WorkDoneProgressOptions {
    resolveProvider?: boolean;
}

interface WorkspaceSymbolRegistrationOptions
    extends WorkspaceSymbolOptions {
}

interface WorkspaceSymbolParams extends WorkDoneProgressParams,
    PartialResultParams {
    query: string;
}

interface WorkspaceSymbol {
    name: string;
    kind: SymbolKind;
    tags?: SymbolTag[];
    containerName?: string;
    location: Location | { uri: DocumentUri };
    data?: LSPAny;
}

interface ConfigurationParams {
    items: ConfigurationItem[];
}

interface ConfigurationItem {
    scopeUri?: URI;
    section?: string;
}

interface DidChangeConfigurationClientCapabilities {
    dynamicRegistration?: boolean;
}

interface DidChangeConfigurationParams {
    settings: LSPAny;
}

interface WorkspaceFoldersServerCapabilities {
    supported?: boolean;
    changeNotifications?: string | boolean;
}

interface WorkspaceFolder {
    uri: URI;
    name: string;
}

interface DidChangeWorkspaceFoldersParams {
    event: WorkspaceFoldersChangeEvent;
}

interface WorkspaceFoldersChangeEvent {
    added: WorkspaceFolder[];
    removed: WorkspaceFolder[];
}

interface FileOperationRegistrationOptions {
    filters: FileOperationFilter[];
}

namespace FileOperationPatternKind {
    file: 'file' = 'file';
    folder: 'folder' = 'folder';
}

type FileOperationPatternKind = 'file' | 'folder';

interface FileOperationPatternOptions {
    ignoreCase?: boolean;
}

interface FileOperationPattern {
    glob: string;
    matches?: FileOperationPatternKind;
    options?: FileOperationPatternOptions;
}

interface FileOperationFilter {
    scheme?: string;
    pattern: FileOperationPattern;
}

interface CreateFilesParams {
    files: FileCreate[];
}

interface FileCreate {
    uri: string;
}

interface RenameFilesParams {
    files: FileRename[];
}

interface FileRename {
    oldUri: string;
    newUri: string;
}

interface DeleteFilesParams {
    files: FileDelete[];
}

interface FileDelete {
    uri: string;
}

interface DidChangeWatchedFilesClientCapabilities {
    dynamicRegistration?: boolean;
    relativePatternSupport?: boolean;
}

interface DidChangeWatchedFilesRegistrationOptions {
    watchers: FileSystemWatcher[];
}

type Pattern = string;

interface RelativePattern {
    baseUri: WorkspaceFolder | URI;
    pattern: Pattern;
}

type GlobPattern = Pattern | RelativePattern;

interface FileSystemWatcher {
    globPattern: GlobPattern;
    kind?: WatchKind;
}

namespace WatchKind {
    Create = 1;
    Change = 2;
    Delete = 4;
}

type WatchKind = uinteger;

interface DidChangeWatchedFilesParams {
    changes: FileEvent[];
}

interface FileEvent {
    uri: DocumentUri;
    type: FileChangeType;
}

namespace FileChangeType {
    Created = 1;
    Changed = 2;
    Deleted = 3;
}

type FileChangeType = 1 | 2 | 3;

interface ExecuteCommandClientCapabilities {
    dynamicRegistration?: boolean;
}

interface ExecuteCommandOptions extends WorkDoneProgressOptions {
    commands: string[];
}

interface ExecuteCommandRegistrationOptions
    extends ExecuteCommandOptions {
}

interface ExecuteCommandParams extends WorkDoneProgressParams {
    command: string;
    arguments?: LSPAny[];
}

interface ApplyWorkspaceEditParams {
    label?: string;
    edit: WorkspaceEdit;
}

interface ApplyWorkspaceEditResult {
    applied: boolean;
    failureReason?: string;
    failedChange?: uinteger;
}

interface ShowMessageParams {
    type: MessageType;
    message: string;
}

namespace MessageType {
    Error = 1;
    Warning = 2;
    Info = 3;
    Log = 4;
    Debug = 5;
}

type MessageType = 1 | 2 | 3 | 4 | 5;

interface ShowMessageRequestClientCapabilities {
    messageActionItem?: {
        additionalPropertiesSupport?: boolean;
    };
}

interface ShowMessageRequestParams {
    type: MessageType;
    message: string;
    actions?: MessageActionItem[];
}

interface MessageActionItem {
    title: string;
}

interface ShowDocumentClientCapabilities {
    support: boolean;
}

interface ShowDocumentParams {
    uri: URI;
    external?: boolean;
    takeFocus?: boolean;
    selection?: Range;
}

interface ShowDocumentResult {
    success: boolean;
}

interface LogMessageParams {
    type: MessageType;
    message: string;
}

interface WorkDoneProgressCreateParams {
    token: ProgressToken;
}

interface WorkDoneProgressCancelParams {
    token: ProgressToken;
}
