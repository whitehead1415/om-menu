(ns it.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [it.views :as views]
   [it.events :as events :refer [listen]]
   [secretary.core :as secretary :include-macros true :refer [defroute]]
   [cljs.core.async :as async :refer [<! chan pub]]
   [om.core :as om :include-macros true]
   [goog.history.EventType :as HistoryEventType]
   [figwheel.client :as fw])
)


(enable-console-print!)


(def app-state 
  (atom 
    {:menu {:current-page 2
            :items [{:title "page1" :page-id :title-view}
                    {:title "page2" :page-id :image-view}
                    {:title "page3" :page-id :list-view}]

            :title-view {:state {:title "This is a title page"}
                         :component views/title-view}
            :image-view {:state {:src "../img/index.jpe"}
                         :component views/image-view}
            :list-view {:state {:items (map (fn [n] {:title (str "List Item #" (+ n 1))}) (take 100 (range)))}
                        :component views/list-view}}}))

(defn app [app-state owner]
  (reify
    om/IRender
    (render [_]
      (om/build views/menu-view (:menu app-state)))))

(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(om/root app app-state
  {:shared {:pub-chan pub-chan
            :notif-chan notif-chan}
   :target (. js/document (getElementById "app")) })

(events/nav-chan)

(secretary/set-config! :prefix "#")

(defroute page-paths "/pages/:idx" [idx]
  (swap! app-state update-in [:menu] assoc :current-page (js/parseInt idx)))

;;; Figwheel
(fw/watch-and-reload
  :jsload-callback (fn []))
