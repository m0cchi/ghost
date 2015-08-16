(ns ghost.core
  (:gen-class main true)
  (:require [ghost.bot-core :as bot]))
 
(defn print-message [data]
 (println (:text data)))

(defn -main [& args]
  (let [token (first args)
        event {:message [#'print-message]}
       bot (bot/constructor-bot token event)]
    (bot/connect bot)))
