
-- About common pitfalls:

-- 1. DeadLocks, never see it coz never use locks
-- 2. LiveLocks, never see it, it's likely come from deadlock resolver, overuse of triggers. 
-- 3. Race Conditions, extremly hard to reproduce n debug, use Mutex to avoid it.

-- Best Practices:

-- 1. Isolate theards as much as possible, minimize the use of shared resources
--    ie: memory, sockets, IO files... This is "avoid locks" strategy to avoiding deadlock, livelock.
-- 2. If it has to use shared resources, use Single writer, as know as non-blocking IO.
--    Queue threads and follow the MVCC patterns (Isolated snapshots)
-- 3. Use Mutex on shared resources to avoid race conditions / data race. In otherword, Synchronization of Threads.
--    In Haskell: takeMVar, putMVar, tryTakeMVar, tryPutMVar
-- 4. Use short-live threads, with cares, always sleeps and kills, or threads will eat your memory


-- Thread/Process

-- Threads use same Address Space, process has its own.
