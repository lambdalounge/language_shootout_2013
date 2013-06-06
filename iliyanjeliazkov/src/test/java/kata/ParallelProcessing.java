package kata;

import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import kata.stats.Statistics;

public class ParallelProcessing implements Strategy {

    private final int concurrency;
    private final ExecutorService pool;

    public ParallelProcessing(int concurrency, ExecutorService pool) {
        this.concurrency = concurrency;
        this.pool = pool;
    }

    public Statistics process(long repetitions, CharBuffer sequence) {
        Collection<Screener> screeners = herd(concurrency, sequence.remaining());
        Screener screener = farm(repetitions, sequence, screeners, pool).process(sequence);
        return screener.stats();
    }

    static Screener farm(long repetitions,
                         CharBuffer sequence,
                         Collection<Screener> screeners,
                         ExecutorService pool) {
        Map<Future<Screener>, Worker> all = new HashMap<Future<Screener>, Worker>();
        for (Screener screener : screeners) {
            Worker worker = new Worker(repetitions, sequence, screener);
            Future<Screener> promise = pool.submit(worker);
            all.put(promise, worker);
        }

        return new ParallelStringCounter(all);
    }

    static Collection<Screener> herd(int n,
                                     int size) {
        int quarter = size / n;
        int reminder = size % n;
        Collection<Screener> screeners = new ArrayList<Screener>();
        for (int i = n; i > 0; i--) {
            int end = i * quarter + (n == 1 ? reminder : 0);
            screeners.add(new Screener((i - 1) * quarter, end));
        }
        return screeners;
    }

    final static class ParallelStringCounter extends Screener {

        private final Map<Future<Screener>, Worker> all;

        public ParallelStringCounter(Map<Future<Screener>, Worker> all) {
            this.all = all;
        }

        @Override
        public Screener process(CharBuffer reader) {
            return this;
        }

        @Override
        public synchronized Statistics stats() {
            assert (all != null && !all.isEmpty());
            try {
                Statistics stats = this.stats;
                for (Future<Screener> p : all.keySet()) {
                    Screener screener = p.get(); // join
                    stats = Statistics.combine(stats, screener.stats());
                }
                return stats;
            } catch (InterruptedException cause) {
                throw new RuntimeException(cause);
            } catch (ExecutionException cause) {
                throw new RuntimeException(cause);
            }
        }

        @Override
        public synchronized long[] counts() {
            assert (all != null);
            try {
                for (Future<Screener> p : all.keySet()) {
                    Screener screener = p.get();
                    counta += screener.counta;
                    countc += screener.countc;
                    countg += screener.countg;
                    countt += screener.countt;
                }
                return super.counts();
            } catch (InterruptedException cause) {
                throw new RuntimeException(cause);
            } catch (ExecutionException cause) {
                throw new RuntimeException(cause);
            }
        }

    }

}
