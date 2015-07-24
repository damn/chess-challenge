(defproject chess_challenge "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [grid2d "0.1.0-SNAPSHOT"]]
  :main chess-challenge.core
  :jvm-opts ["-Xms4G", "-Xmx4G"])
