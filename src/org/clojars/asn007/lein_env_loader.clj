(ns org.clojars.asn007.lein-env-loader
  (:require [clojure.java.io :as io]
            [leiningen.core.eval :as eval]
            [leiningen.core.main :as main]
            [robert.hooke :as hooke]
            [clojure.string :as str]))

(defn- read-env-file
  [file]
  (slurp (io/file file)))

(defn file-exists? [path] (.exists (io/file path)))

(defn env-exists?
  ([name]
   (->
     (System/getenv name)
     (= nil)
     not)))

(defn str-join-newline! [x & rest]
  (str/join "\n" (concat x rest)))

(defn str-split-first
  ([re s]
   (str/split s re 2))
  ([re]
   (fn [s]
     (str/split s re 2))))

(defn collect-env-files
  ([path]
   (let [base-path (str/replace (.getCanonicalPath (io/file path)) (char \\) (char \/))]
     (println "-> using base path" base-path)
     (as-> base-path processed
           (str/split processed #"\/")
           (map-indexed vector processed)
           (map
             (fn [[i x]]
               (str (str/join "/" (map last (take (+ i 1) processed))) "/.env"))
             processed)
           (filter file-exists? processed))))
  ([] (collect-env-files ".")))

(defn load-all-env-vars
  ([path]
   (into {}
         (vec
           (filter
             #(not (env-exists? (first %)))
             (map (str-split-first #"=")
                  (-> (map read-env-file (collect-env-files path))
                      (str-join-newline!)
                      str/split-lines))))))
  ([] (load-all-env-vars ".")))

(defmacro with-env-vars-in [project task-name & body]
  `(binding [eval/*env* (load-all-env-vars ".")]
     ~@body))


(defn ^:no-project-needed ^:higher-order with-env-vars
  "Perform a task with environment variable settings loaded from project.clj.
Specify your environment variables with :env-vars key in the project.clj.
If you instead specify a string or vector of strings to that key, they will be
viewed as the name of files containing environment variable settings."
  [project task-name & args]
  (try
    (with-env-vars-in project task-name
                      (let [task-name (main/lookup-alias task-name project)]
                        (main/apply-task task-name project args)))
    (catch Exception e#
      (main/info (format "Error encountered performing task '%s'" ~task-name))
      (if (and (:exit-code (ex-data e#)) (not main/*debug*))
        (main/info (.getMessage e#))
        (.printStackTrace e#)))))

(defn- inject-env-vars [f task-name project args]
  (with-env-vars-in project task-name
                    (f task-name project args)))

(defn auto-inject
  "A plugin hook to automatically inject environment variables for every task execution."
  []
  (hooke/add-hook #'leiningen.core.main/apply-task #'inject-env-vars))
