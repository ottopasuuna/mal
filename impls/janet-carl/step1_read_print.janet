(import ./reader)
(import ./printer)

(defn READ [str]
  (reader/read_str str))

(defn EVAL [ast]
  ast)

(defn PRINT [ast]
  (printer/pr_str ast))

(defn rep [str]
 (try
  (-> str
      (READ)
      (EVAL)
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
