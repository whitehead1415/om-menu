(ns it.graphics
  (:require
   [om.core :as om :include-macros true]))

(defn animate [owner attr target-value duration ease]
  (let [interpolator (.interpolate js/d3 (om/get-state owner attr), target-value)]
    (-> (.transition js/d3)
      (.duration 200)
      (.ease "cubic-out")
      (.tween attr (fn [] (fn [t] (om/set-state! owner attr (interpolator t))))))
    ))
