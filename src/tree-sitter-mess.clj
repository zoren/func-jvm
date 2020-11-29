(ns parsing
  (:import
   [jsitter.api Parser Language Tree Node Zipper NodeType Terminal]
   )
  )

(defn node-type [^Node node]
  (when node (.getType node)))

(defn byte-size [^Node node]
  (.getByteSize node))

(defn root [^Tree tree]
  (when tree (.getRoot tree)))

(defn zipper [^Node node]
  (when node (.zipper node)))

(defn byte-offset [^Zipper z]
  (.getByteOffset z))

(defn byte-range [z]
  (let [o (byte-offset z)]
    [o (+ o (byte-size z))]))

(defn node [^Zipper z]
  (when z (.getNode z)))

(defn up [^Zipper z]
  (when z (.up z)))

(defn down [^Zipper z]
  (when z (.down z)))

(defn right [^Zipper z]
  (when z (.right z)))

(defn left [^Zipper z]
  (when z (.left z)))

(defn next [^Zipper z]
  (when z (.next z)))

(defn skip [^Zipper z]
  (when z (.skip z)))

(defn alias [^Zipper z]
  (when z (.getAlias z)))

(defn node-type-name [^NodeType t]
  (.getName t))

(defn terminal? [t]
  (cond
    (instance? NodeType t) (instance? Terminal t)
    (instance? Tree t) (recur (.getRoot ^Tree t))
    (instance? Node t) (recur (.getType ^Node t))))

(defn- s-expr-impl [^Zipper z]
  (let [node-type ^NodeType (or (.getAlias z)
                                (.getType (.getNode z)))]
    (if (instance? Terminal node-type)
      (.getName node-type)
      (lazy-seq (let [child (.down z)]
                  (apply list (symbol (.getName node-type))
                         (map s-expr-impl (take-while some? (iterate #(.right ^Zipper %) child)))))))))

(defn s-expr [x]
  (cond
    (instance? Zipper x) (s-expr-impl x)
    (instance? Tree x) (recur (.getRoot ^Tree x))
    (instance? Node x) (recur (.zipper ^Node x))
    :else nil))

(defn parse-str [lang-or-parser s]
  (if (instance? Language lang-or-parser)
    (recur (.parser ^Language lang-or-parser) s)
    (.parse ^Parser lang-or-parser (jsitter.api.StringText. s) nil nil)))

(defn invoke-private-method [obj fn-name-string & args]
  (let [m (first (filter (fn [x] (.. x getName (equals fn-name-string)))
                         (.. obj getClass getDeclaredMethods)))]
    (. m (setAccessible true))
    (. m (invoke obj args))))

(defn private-field [obj fn-name-string]
  (let [m (.. obj getClass (getDeclaredField fn-name-string))]
    (. m (setAccessible true))
    (. m (get obj))))

(comment
  ;;public static jsitter.api.Language jsitter.api.Language.load(jsitter.api.NodeType,java.lang.String,java.lang.String,java.lang.String,java.lang.ClassLoader)
  (map #(.getName %) (.getMethods Language))
  ;;  ("getName" "register" "load" "parser" "getRootNodeType" "nodeType")
  ;;https://github.com/JetBrains/jsitter/blob/master/src/test/kotlin/jsitter/test/tests.kt
  ;;val lang = Language.load(SourceFile, "go", "tree_sitter_go", "libtsgo.dylib", Language::class.java.classLoader)
  (-> Language .getClassLoader)
  (def src (NodeType. "source_file"))
  (def lib
    (com.sun.jna.NativeLibrary/getInstance  "/Users/zoren/deon/tree-sitter-csl/a.out" (.getClassLoader Language)))
  (private-field lib "functions")

  (def lang
    (Language/load src "csl" "tree_sitter_CSL" "/Users/zoren/deon/tree-sitter-csl/a.out" (.getClassLoader Language)))
  (.getName lang)
  (class lang)
  (instance? Language lang)
  (instance? Tree src)
  (def parser (.parser lang))
  (.parse parser (jsitter.api.StringText. "val x = 5") nil)
  (map #(.getName %) (.getMethods (class parser)))
  (filter #(= (.getName %) "parse") (.getMethods (class parser)))
  
  (.getMethods )
  (parse-str lang "val x = 5")
  )
