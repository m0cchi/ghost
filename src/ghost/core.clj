(ns ghost.core
  (:gen-class main true)
  (:require [clj-slack.rtm :as rtm]
            [gniazdo.core :as ws]))

(defn receiver [mes]
  (println mes))

(defn connect [url]
  (let [s (ws/connect url
                      :on-receive #'receiver)]))

(defn -main [& args]

  (let [token (first args)
        connection  {:api-url "https://slack.com/api" :token token}]
    (connect (get (rtm/start connection) :url))))
