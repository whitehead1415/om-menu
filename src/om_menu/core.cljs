(ns om-menu.core
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [figwheel.client :as fw]))

(enable-console-print!)

;;; Model

(def app-state (atom {:menu {:list [{:title "page1" :action "page1"}
                                    {:title "page2" :action "foo"}
                                    {:title "page3" :action "foo"}]}}))

;;; View

(defn list-item [item owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (:title item)))))

(defn list-view [items owner]
  (reify
    om/IRender
    (render [this]
      (apply dom/ul nil
             (om/build-all list-item items)))))

(defn image-view [image owner]
  (reify
    om/IRender
    (render [this]
      (dom/img #js{:src (:src image)} nil))))

(defn title-view [title owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil title))))

(defn menu [menu-state owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js{:className "menu"}
               (apply dom/ul nil
                      (om/build-all list-item (:list (:menu menu-state))))))))

(defn app [app-state owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil 
               ;(om/build page ((:current-page app-state) (:pages app-state)))
               (om/build menu app-state)))))

(om/root app app-state
         {:target (. js/document (getElementById "app")) })


;;; Figwheel
(fw/watch-and-reload
 :jsload-callback (fn []))
