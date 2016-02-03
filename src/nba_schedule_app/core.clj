(ns nba-schedule-app.core
  (:require [clojure.string :as str]
            [clj-http.client :as client]
            [clj-time.local :as local])
  (:gen-class))

(def url-template "http://stats.nba.com/stats/scoreboard/?GameDate=%&LeagueID=00&DayOffset=0")

(defn today []
  "returns string of form MM/DD/YYYY representing today's date"
  (let [now (.toString (local/local-now))
        date (str/split now #"-|T")]
    (str (date 1) "/" (date 2) "/" (date 0))))

(defn get-schedule [date]
  "queries stats.nba.com and returns the body of the response json parsed as a vector"
  (let [url (str/replace url-template #"%" date)]
    (try 
      (client/json-decode (:body (client/get url)))
        (catch 
          Exception e (println "server request error -- check your parameters, such as date")))))


(defn map-names [schedule]
  (let [names (map #(get %1 "name") schedule)]
    (zipmap names schedule)))

(defn team-id-map [schedule]
  "return of a mapping from nba team ids to their cities"
  (defn add-teams [conference headers teams]
    (doseq [team (get conference "rowSet")]
      (let [team-data (zipmap headers team)]
        (swap! teams assoc (get team-data "TEAM_ID") (get team-data "TEAM")))))
  (let [teams (atom {})
        tables (map-names schedule)
        east (get tables "EastConfStandingsByDay")
        west (get tables "WestConfStandingsByDay")
        headers (get east "headers")]
    (add-teams east headers teams)
    (add-teams west headers teams)
  @teams))

(defn get-games [date]
  (let [matchups (atom [])
        schedule (get (get-schedule date) "resultSets")
        team-ids (team-id-map schedule)
        games (get (map-names schedule) "GameHeader")
        headers (get games "headers")]
    (doseq [matchup (get games "rowSet")]
      (let [matchup-data (zipmap headers matchup)
            {home-id "HOME_TEAM_ID" 
             visitor-id "VISITOR_TEAM_ID" 
             status "GAME_STATUS_TEXT"
             tv "NATL_TV_BROADCASTER_ABBREVIATION"} matchup-data
            {home home-id visit visitor-id} team-ids]
        (swap! matchups conj [visit "@" home status (or tv "Local TV")])))
    @matchups))

(defn game-contains-team? [team]
  (fn [[away at home]] (or (= team away) (= team home))))



(defn -main

  ([]
    (-main (today)))

  ([date]
   (let [games (get-games date)]
     (if (empty? games) 
       (println "no games scheduled")
       (doseq [g games]
         (println g)))))

  ([date team]
   (let [games (get-games date)
         team-games (filter (game-contains-team? team) games)]
     (if (empty? team-games) 
         (println "no games scheduled")
           (doseq [g team-games]
             (println g))))))

