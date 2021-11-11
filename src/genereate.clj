(ns genereate
  (:import [clojure.asm Opcodes Type ClassWriter]
           [clojure.asm.commons Method GeneratorAdapter])
  )


                                        ;(Method/getMethod "static <T> Class<?  extends T> m (int n)")
;; https://gist.github.com/athos/1033052/
;; https://clojureverse.org/t/generating-reflection-free-java-wrappers/4421
(defn make-class [name]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        init (Method/getMethod "void <init>()")
        meth (Method/getMethod "long fact(long)")]
    (.visit cw Opcodes/V1_6 Opcodes/ACC_PUBLIC (.replace name \. \/) nil "java/lang/Object" nil)
    (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC init nil nil cw)
      (.visitCode)
      (.loadThis)
      (.invokeConstructor (Type/getType Object) init)
      (.returnValue)
      (.endMethod))
    (let [mv (GeneratorAdapter. Opcodes/ACC_PUBLIC meth nil nil cw)
          LONG (Type/getType Long/TYPE)
          acc (.newLocal mv LONG)
          ;; beginning (.newLabel mv)
          ;; then (.newLabel mv)
          ]
      (doto mv
        (.visitCode)
        (.loadArg 0)
        (.push 54)
        (.math GeneratorAdapter/MUL LONG)
        ;; (.mark beginning)
        ;; (.visitCode)
        ;; (.loadArg 0)
        ;; (.ifZCmp GeneratorAdapter/NE then)
        ;; (.loadLocal acc)
        ;; (.returnValue)
        ;; (.mark then)
        ;; (.loadArg 0)
        ;; (.dup)
        ;; (.loadLocal acc)
        ;; (.math GeneratorAdapter/MUL INT)
        ;; (.storeLocal acc)
        ;; (.push 1)
        ;; (.math GeneratorAdapter/SUB INT)
        ;; (.storeArg 0)
        ;; (.goTo beginning)
        (.returnValue)
        (.endMethod)))
    (.visitEnd cw)
    (let [b (.toByteArray cw)
          cl (clojure.lang.DynamicClassLoader.)]
      (.defineClass cl name b nil))))

(comment
  (make-class "foo.Foo")
  foo.Foo
  (def o (construct-proxy *1))
  (.fact o 6)

  (proxy [] []
    (getF1 [this] f1)
    (getF2 [this] f2))
  (Tuple2. 3 4)
  (def t2 *1)
  (bean t2)
  ;; #'user/o
  ;; user=> (.fact o 6)
  ;; 720
  ;; user=>
  )
