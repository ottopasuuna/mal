
(defn READ [str]
  str)

(defn EVAL [str]
  str)

(defn PRINT [str]
  str)

(defn rep [str]
  (-> str
      (READ)
      (EVAL)
      (PRINT)))

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
      (prin (rep buf))
      (break))))
