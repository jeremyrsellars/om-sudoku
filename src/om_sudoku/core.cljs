(ns om-sudoku.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-sudoku.sudoku :as sudoku :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)


(defn better-board [b]
  (letfn [(better-cell [coord]
            {:value (if (sudoku/has-value? b coord) (sudoku/value-at b coord))
             :has-value (sudoku/has-value? b coord)
             :values (->> (sudoku/valid-values-for b coord)
                          (interpose ",")
                          (apply str))})
          (better-row [r cells]
            (apply vector (map-indexed (fn [c _] (better-cell [r c])) cells)))
          (better-rows []
            (vec (map-indexed better-row b)))]
    (better-rows)))

sudoku/sample-board

(better-board sudoku/sample-board)


(def app-state
  (atom
    {:board {:rows sudoku/sample-board
             :all-values sudoku/all-values
             :value-count (count sudoku/all-values)}}))

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn handle-change [e board coord]
  (let [s (.. e -target -value)
        new-value (if (empty? s) 0 (js/parseInt s))]
    (om/transact!
      board
      :rows
      (fn [_]
        (sudoku/set-value-at (get-board @board) coord new-value)))))

(defn end-edit [text owner cb]
  (om/set-state! owner :editing false)
  (cb text))

(defn commit-change [text owner]
  (om/set-state! owner :editing true))

(def get-board :rows)

(defn sudoku-cell [board owner {:keys [coord] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:editing true})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      (let [b           (get-board board)
            text        (when (sudoku/has-value? b coord) (sudoku/value-at b coord))
            placeholder (->> (sudoku/valid-values-for b coord)
                          (interpose ",")
                          (apply str))]
        (dom/div nil
          (dom/span #js {:style (display (not editing))} text)
          (dom/input
            #js {:style (display editing)
                 :value text
                 :placeholder placeholder
                 :onChange #(handle-change % board coord)
                 :onKeyPress #(when (== (.-keyCode %) 13)
                                (end-edit text owner on-edit))
                 #_:onBlur #_(fn [e]
                           (when (om/get-state owner :editing)
                             (end-edit text owner on-edit)))
                 })
          (dom/button
            #js {:style (display (not editing))
                 :onClick #(om/set-state! owner :editing true)}
            "Edit"))))))

(defn board-view [board owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/table nil
        (for [row-index (range (:value-count board))]
          (apply dom/tr nil
            (for [col-index (range (:value-count board))]
              (dom/td nil
                 (om/build sudoku-cell board {:opts {:coord [row-index col-index]}})))))))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (om/build board-view (:board app)))))

(om/root app-view app-state
  {:target (. js/document (getElementById "board"))})
