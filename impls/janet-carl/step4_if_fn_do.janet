(import ./reader)
(import ./printer)

(def TRACE_LOG false)

(defn make-trace-logger [name]
  (fn trace-logger [mesg]
    (do
      (if TRACE_LOG
        (print (if (string? mesg)
                 (string/format "%s: %s" name mesg)
                 (string/format "%s: %s" name (printer/pr_str mesg)))))
      mesg)))

(def eval-trace (make-trace-logger "EVAL-TRACE"))

(defn READ [str]
  (reader/read_str str))

(defn EVAL [ast env]
  (defn eval_ast [ast env]
    (eval-trace (string/format "eval_ast: ast=%s" (printer/pr_str ast)))
    (cond
      (string? ast) (let [val (get env ast)]
                      (if (nil? val)
                        (error (string "Symbol " ast " not found"))
                        val))
      (array?  ast) (map (fn [e]
                           (EVAL e env))
                         ast)
      ast))

  (eval-trace ast)
  (if (array? ast)
    (if (empty? ast)
      ast
      (case (ast 0)
        "def!" (let [symb     (ast 1)
                     val_ast  (ast 2)]
                 # update the environment
                 (set (env symb) (EVAL val_ast env)))
        "let*" (let [newenv   (table/setproto @{} env)
                     bindings (let [elems (ast 1)
                                    nelems (length elems)
                                    bndings @[]]
                                (if (not (= 0 (% nelems 2)))
                                  (error "Bad bindings"))
                                (loop [i :range [0 nelems 2]]
                                  (put bndings (/ i 2) [(elems i) (elems (+ 1 i))]))
                                bndings)
                     body     (ast 2)]
                 (eval-trace (string/format "let* bindings: %v" bindings))
                 (each [binding expr] bindings
                   (put newenv binding (EVAL expr newenv)))
                 (EVAL body newenv))
        "do" (last (seq [expr :in (array/slice ast 1)]
                     (EVAL expr env)))
        "if" (let [condition (ast 1)
                   true-arm  (ast 2)
                   false-arm (if (> (length ast) 3)
                               (ast 3)
                               nil)
                   ev-condition (EVAL condition env)]
               (if ev-condition
                 (EVAL true-arm env)
                 (if (not (nil? false-arm))
                   (EVAL false-arm env))))
        (let [[f & args] (eval_ast ast env)]
          (eval-trace (string/format "function application (%v %v)" f args))
          (apply f args))))
    (eval_ast ast env)))

(defn PRINT [ast]
  (printer/pr_str ast))


(def repl_env @{"+" (fn [a b] (+ a b))
                "-" (fn [a b] (- a b))
                "*" (fn [a b] (* a b))
                "/" (fn [a b] (/ a b))
                "list" array
                "list?" array?
                "empty?" empty?
                "count" (fn [list]
                          (if (nil? list)
                            0
                            (length list)))
                "=" deep=
                ">" >
                "<" <
                ">=" >=
                "<=" <=})


(defn rep [str]
(-> str
        (READ)
        (EVAL repl_env)
        (PRINT))
  # (try
  #   (-> str
  #       (READ)
  #       (EVAL repl_env)
  #       (PRINT))
  #   ([err] (print err)))
  )

# Ended up copying these from existing implementation...
# Seems due to a quirk in Janet's getline
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn main
  [& args]
  (var buf nil)
  (while true
    (set buf @"")
    (getstdin "user> " buf)
    (if (< 0 (length buf))
      (print (rep buf))
      (break))))
