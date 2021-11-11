(ns language-server
  "
  https://github.com/snoe/clojure-lsp/blob/master/deps.edn
  https://github.com/snoe/clojure-lsp/blob/master/src/clojure_lsp/handlers.clj
  https://github.com/snoe/clojure-lsp/blob/master/src/clojure_lsp/main.clj
  "
  (:require
   [clojure.tools.logging :as log])
  (:import
   (org.eclipse.lsp4j.services
    LanguageServer
    TextDocumentService
    WorkspaceService)
   (org.eclipse.lsp4j
    DidChangeConfigurationParams
    DidChangeTextDocumentParams
    DidChangeWatchedFilesParams
    DidCloseTextDocumentParams
    DidOpenTextDocumentParams
    DidSaveTextDocumentParams
    ExecuteCommandParams
    HoverParams
    InitializeParams
    InitializedParams
    WorkspaceSymbolParams
    )
   (org.eclipse.lsp4j.launch LSPLauncher)
   (java.util.concurrent CompletableFuture))
  (:gen-class))

(deftype LSPTextDocumentService []
  TextDocumentService
  (^void didOpen [_ ^DidOpenTextDocumentParams params]
   )

  (^void didChange [_ ^DidChangeTextDocumentParams params]
   )

  (^void didSave [_ ^DidSaveTextDocumentParams _params]
   )

  (^void didClose [_ ^DidCloseTextDocumentParams params]
   )

  (^CompletableFuture hover [this ^HoverParams params]
   )
  )

(deftype LSPWorkspaceService []
  WorkspaceService

  (^CompletableFuture executeCommand [_ ^ExecuteCommandParams params]
   (CompletableFuture/completedFuture 0))

  (^void didChangeConfiguration [_ ^DidChangeConfigurationParams params]
   (log/warn params))

  (^void didChangeWatchedFiles [_ ^DidChangeWatchedFilesParams params]
   (log/warn "DidChangeWatchedFilesParams")
   )

  (^CompletableFuture symbol [this ^WorkspaceSymbolParams params]
   ))

(def server
  (proxy [LanguageServer] []
    (^CompletableFuture initialize [^InitializeParams params]
     (prn "init")
     )
    (^void initialized [^InitializedParams params]
     )
    (^CompletableFuture shutdown []
     (log/info "Shutting down")

     (CompletableFuture/completedFuture
      {:result nil}))
    (exit []
      (log/info "Exit")
      (shutdown-agents)
      (System/exit 0))
    (getTextDocumentService []
      (LSPTextDocumentService.))
    (getWorkspaceService []
      (LSPWorkspaceService.))))

(defn- run []
  (prn "main")
  (log/info "Starting server...")
  (let [launcher (LSPLauncher/createServerLauncher server System/in System/out)]
    (.startListening launcher)))

(defn -main [& args]
  (if (empty? args)
    (run)
    (println "no args expected" )))
