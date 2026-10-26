(ns jepsen.generator.test
  "This namespace contains functions for testing generators. See the
  `jepsen.generator-test` namespace in the `test/` directory for a concrete
  example of how these functions can be used.

  NOTE: While the `simulate` function is considered stable at this point, the
  others might still be subject to change -- use with care and expect possible
  breakage in future releases."
  (:require [clojure.datafy :refer [datafy]]
            [dom-top.core :refer [assert+]]
            [jepsen [generator :as gen]
                    [history :as history]
                    [random :as rand]]
            [jepsen.generator.context :as ctx])
  (:import (io.lacuna.bifurcan Set)))

(def default-test
  "A default test map."
  {:nodes ["n1" "n2"]})

(defn n+nemesis-context
  "A context with n numeric worker threads and one nemesis."
  [n]
  (gen/context {:concurrency n}))

(def default-context
  "A default initial context for running these tests. Two worker threads, one
  nemesis."
  (n+nemesis-context 2))

(defn invocations
  "Only invokes, not returns"
  [history]
  (filter #(= :invoke (:type %)) history))

(defmacro with-fixed-rands
  "Rebinds jepsen.random and clojure's rand, rand-int, and rand-nth to yield a
  deterministic series of random values. Definitely not threadsafe, but fine
  for tests I think."
  [seed & body]
  `(let [rand-values#     (atom (gen/rand-seq ~seed))
         rand-int-values# (atom (gen/rand-int-seq ~seed))
         rand#      (fn ~'rand
                      ([]
                       (first (swap! rand-values# next)))
                      ([limit#]
                       (* limit# (first (swap! rand-values# next)))))
         rand-int#  (fn ~'rand-int [limit#]
                      (if (zero? limit#)
                        0
                        (mod (first (swap! rand-int-values# next))
                             limit#)))
         rand-nth#   (fn ~'rand-nth [coll#]
                       (nth coll# (rand-int# (count coll#))))]
     (rand/with-seed ~seed
       (with-redefs [rand     rand#
                     rand-int rand-int#
                     rand-nth rand-nth]
         ~@body))))

(def rand-seed
  "We need tests to be deterministic for reproducibility, but also
  pseudorandom. Changing this seed will force rewriting some tests, but it
  might be necessary for discovering edge cases."
  45100)

(defn simulate
  "Simulates the series of operations obtained from a generator, given a
  function that takes a context and op and returns the completion for that op.

  Strips out op :index fields--it's generally not as useful for testing."
  ([gen complete-fn]
   (simulate default-context gen complete-fn))
  ([ctx gen complete-fn]
   (simulate default-test ctx gen complete-fn))
  ([test ctx gen complete-fn]
   (with-fixed-rands rand-seed
     (loop [ops        []
            in-flight  [] ; Kept sorted by time
            gen        (gen/validate gen)
            ctx        ctx]
       ;(binding [*print-length* 3] (prn :invoking :gen gen))
       (let [[invoke gen'] (gen/op gen test ctx)]
         ; (prn :invoke invoke :in-flight in-flight)
         (cond
           ; We're done
           (nil? invoke)
           (->> (into ops in-flight)
                (history/strip-indices))

           ; TODO: the order of updates for worker maps here isn't correct; fix
           ; it.
           ; We have an invocation that's not pending, and that invocation is
           ; before every in-flight completion
           (and (not= :pending invoke)
                (or (empty? in-flight)
                    (<= (:time invoke) (:time (first in-flight)))))
           (let [thread (gen/process->thread ctx (:process invoke))
                 ; Advance clock, mark thread as busy
                 ctx'   (ctx/busy-thread
                          ctx
                          (max (:time ctx) (:time invoke))
                          thread)]
             (case (:type invoke)
               (:invoke, :info)
               (let [; Update the generator with this invocation
                     gen'      (gen/update gen' test ctx' invoke)
                     ; Add the completion to the in-flight set
                     ;_         (prn :invoke invoke)
                     complete  (complete-fn ctx' invoke)
                     in-flight (sort-by :time (conj in-flight complete))]
                 (recur (conj ops invoke) in-flight gen' ctx'))

               ; For logs... you *might* want to see these so you can test
               ; logging, but log ops don't actually appear in the history, so
               ; I think the thing to do is to silently drop them.
               :log (recur ops in-flight gen' ctx)

               ; Sleeps are weird: the thread is busy, but we don't put
               ; anything in the history.
               :sleep
               (let [complete (assoc invoke :time (+ (:time invoke)
                                                     (:value invoke)))
                     in-flight (sort-by :time (conj in-flight complete))]
                 (recur ops in-flight gen' ctx'))))

           ; We need to complete something before we can apply the next
           ; invocation.
           :else
           (let [gen'   (if (identical? :pending invoke)
                          ; The generator was pending; we "advanced time" but
                          ; let the generator step.
                          gen'
                          ; The generator was not pending; we did not consume
                          ; anything from it, and it should remain unchanged.
                          gen)
                 op     (first in-flight)
                 _      (assert+ op
                                 {:type :generator-pending-but-nothing-in-flight
                                  :gen gen'
                                  :ctx (datafy ctx)})
                 thread (gen/process->thread ctx (:process op))
                 ; Advance clock, mark thread as free
                 ctx    (ctx/free-thread ctx (:time op) thread)]
             (case (:type op)
               :sleep
               (recur ops (rest in-flight) gen ctx)

               ; Normal completion
               (:log :ok :info :fail)
               (let [; Update generator with completion
                     gen'   (gen/update gen' test ctx op)
                     ; Update worker mapping if this op crashed
                     ctx    (if (or (= :nemesis thread) (not= :info (:type op)))
                              ctx
                              (ctx/with-next-process ctx thread))]
                 (recur (conj ops op) (rest in-flight) gen' ctx))))))))))

(defn quick-ops
  "Simulates the series of ops obtained from a generator where the
  system executes every operation perfectly, immediately, and with zero
  latency."
  ([gen]
   (quick-ops default-context gen))
  ([ctx gen]
   (quick-ops default-test ctx gen))
  ([test ctx gen]
   (simulate test ctx gen (fn [ctx invoke] (assoc invoke :type :ok)))))

(defn quick
  "Like quick-ops, but returns just invocations."
  ([gen]
   (quick default-context gen))
  ([ctx gen]
   (quick default-test ctx gen))
  ([test ctx gen]
   (invocations (quick-ops test ctx gen))))

(def perfect-latency
  "How long perfect operations take"
  10)

(defn perfect*
  "Simulates the series of ops obtained from a generator where the system
  executes every operation successfully in 10 nanoseconds. Returns full
  history."
  ([gen]
   (perfect* default-context gen))
  ([ctx gen]
   (perfect* default-test ctx gen))
  ([test ctx gen]
   (simulate test ctx gen
             (fn [ctx invoke]
               (-> invoke
                   (assoc :type :ok)
                   (update :time + perfect-latency))))))

(defn perfect
  "Simulates the series of ops obtained from a generator where the system
  executes every operation successfully in 10 nanoseconds. Returns only
  invocations."
  ([gen]
   (perfect default-context gen))
  ([ctx gen]
   (perfect default-test ctx gen))
  ([test ctx gen]
   (invocations (perfect* test ctx gen))))

(defn perfect-info
  "Simulates the series of ops obtained from a generator where every operation
  crashes with :info in 10 nanoseconds. Returns only invocations."
  ([gen]
   (perfect-info default-context gen))
  ([ctx gen]
   (perfect-info default-test ctx gen))
  ([test ctx gen]
   (invocations
     (simulate test ctx gen
               (fn [ctx invoke]
                 (-> invoke
                     (assoc :type :info)
                     (update :time + perfect-latency)))))))

(defn imperfect
  "Simulates the series of ops obtained from a generator where threads
  alternately fail, info, then ok, and repeat, taking 10 ns each. Returns
  invocations and completions."
  ([gen]
   (imperfect default-context gen))
  ([ctx gen]
   (imperfect default-test ctx gen))
  ([test ctx gen]
   (let [state (atom {})]
     (simulate test ctx gen
               (fn [ctx invoke]
                 (let [t (gen/process->thread ctx (:process invoke))]
                   (-> invoke
                       (assoc :type (get (swap! state update t {nil   :fail
                                                                :fail :info
                                                                :info :ok
                                                                :ok   :fail})
                                         t))
                       (update :time + perfect-latency))))))))
