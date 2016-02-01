(ns nba-schedule-app.core
  (:require [clojure.string :as str]
            [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-time.local :as local])
  (:gen-class))

(def url-template "http://stats.nba.com/stats/scoreboard/?GameDate=%&LeagueID=00&DayOffset=0")

(defn today []
  "returns string of form MM/DD/YYYY representing today's date"
  (let [now (.toString (local/local-now))
        date (str/split now #"-|T")]
    (str (date 1) "/" (date 2) "/" (date 0))))

(defn stats-for-date [date]
  "queries stats.nba.com and returns the body of the response json parsed as a vector"
  (let [url (str/replace url-template #"%" date)]
    (get (json/parse-string (:body (client/get url))) "resultSets")))



(defn -main
  [& args]
  (def stats (stats-for-date (today)))
  (println stats))

