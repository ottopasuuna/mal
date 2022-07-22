(import ./reader)
(import ./printer)



(defn READ [str]
  (reader/read_str str))

(defn EVAL [ast env]
  (defn eval_ast [ast env]
    (cond
      (string? ast) (get env ast)
      (array?  ast) (map (fn [e]
                           (EVAL e env))
                         ast)
      ast))
  (if (array? ast)
    (if (empty? ast)
      ast
      (let [[f & args] (eval_ast ast env)]
        (apply f args)))
    (eval_ast ast env)))

(defn PRINT [ast]
  (printer/pr_str ast))

(def repl_env {"+" (fn [a b] (+ a b))
               "-" (fn [a b] (- a b))
               "*" (fn [a b] (* a b))
               "/" (fn [a b] (/ a b))})

(defn rep [str]
 (try
  (-> str
      (READ)
      (EVAL repl_env)
      (PRINT))
  ([err] (print err))))

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
