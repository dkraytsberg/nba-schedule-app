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

(defn get-schedule-results [date]
  (get (get-schedule date) "resultSets"))

(defn map-names-over-tables [tables]
  "return a map with each table's name as the key and the table as value"
  (let [names (map #(get %1 "name") tables)]
    (zipmap names tables)))

(defn team-id-map [schedule]
  "return of a mapping from nba team ids to their cities"
  (defn add-teams [conference headers teams]
    (doseq [team (get conference "rowSet")]
      (let [team-data (zipmap headers team)]
        (swap! teams assoc (get team-data "TEAM_ID") (get team-data "TEAM")))))
  (let [teams (atom {})
        tables (map-names-over-tables schedule)
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
        games (get (map-names-over-tables schedule) "GameHeader")
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

(defn get-games-2 [date]
  (let [results (atom [])
        schedule (get-schedule-results date)
        team-ids (team-id-map schedule)
        games (get (map-names-over-tables schedule) "GameHeader")
        headers (get games "headers")]
    (doseq [matchup (get games "rowSet")]
      (let [matchup-data (zipmap headers matchup)
            {game-id "GAME_ID"
             home-id "HOME_TEAM_ID" 
             visit-id "VISITOR_TEAM_ID" 
             status "GAME_STATUS_TEXT"
             tv "NATL_TV_BROADCASTER_ABBREVIATION"} matchup-data
            {home home-id 
             visit visit-id} team-ids
             game-map {:game-id game-id
                       :visit-id visit-id
                       :visit visit
                       :home-id home-id
                       :home home
                       :status status
                       :tv (or tv "Local Brodcast")}]
        (swap! results conj game-map)))
    @results))


(defn get-scores [date]
  "return a mapping from nba game ids to their score data"
  (let [results (atom [])
        schedule (get-schedule-results date)
        team-ids (team-id-map schedule)
        scores (get (map-names-over-tables schedule) "LineScore")
        headers (get scores "headers")]
    (doseq [game-scores (get scores "rowSet")]
      (let [score-data (zipmap headers game-scores)
            {game-id "GAME_ID"
             team-id "TEAM_ID"
             pts "PTS"
             fg% "FG_PCT"
             ft% "FT_PCT"
             fg3% "FG3_PCT"
             ast "AST"
             reb "REB"} score-data
            {team team-id} team-ids
            score-map {:game-id game-id
                       :team-id team-id
                       :team team
                       :pts pts
                       :fg% fg%
                       :fg3% fg3%
                       :ast ast
                       :reb reb}]
        (swap! results conj score-map)))
    @results))

(defn merge-games-with-scores [games scores]
  (let [results (atom [])]
    (doseq [game games]
      (assoc games (filter #(


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

