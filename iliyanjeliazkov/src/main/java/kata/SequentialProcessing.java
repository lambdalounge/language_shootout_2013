package kata;

import java.nio.CharBuffer;

import kata.stats.Statistics;

public class SequentialProcessing implements Strategy {

    public Statistics process(long repetitions,
                            CharBuffer sequence) {
        Worker worker = new Worker(repetitions, sequence, new Screener());
        try {
            Screener screener = worker.call();
            return screener.stats();
        } catch (Exception cause) {
            throw new RuntimeException(cause);
        }
    }

}
