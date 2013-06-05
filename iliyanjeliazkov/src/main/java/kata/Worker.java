package kata;

import java.nio.CharBuffer;
import java.util.concurrent.Callable;

final class Worker implements Callable<Screener> {
    private final CharBuffer sequence;
    private final Screener screener;
    private final long repetitions;

    Worker(long repetitions, CharBuffer sequence, Screener screener) {
        this.sequence = sequence;
        this.repetitions = repetitions;
        this.screener = screener;
    }

    public Screener call() throws Exception {
        for (long i = 0; i < repetitions; i++) {
            sequence.rewind();
            screener.process(sequence);
        }
        return screener;
    }

}