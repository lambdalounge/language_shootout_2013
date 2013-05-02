package kata;

import java.nio.CharBuffer;

import kata.stats.Statistics;

public class Screener {

    long counta;
    long countc;
    long countg;
    long countt;

    final Statistics stats = new Statistics(System.nanoTime(), "ns");

    private final int start;
    private final int end;

    public Screener(int start, int end) {
        this.start = start;
        this.end = end;
    }

    public Screener() {
        this(0, Integer.MAX_VALUE);
    }

    public Screener process(CharBuffer sequence) {
        CharBuffer segment = sequence.subSequence(start, Math.min(end, sequence.remaining()));
        while (segment.hasRemaining()) {
            char c = segment.get();
            switch (c) {
                case 'A':
                    counta++;
                    break;
                case 'C':
                    countc++;
                    break;
                case 'G':
                    countg++;
                    break;
                case 'T':
                    countt++;
                    break;
                default:
                    throw new IllegalArgumentException("Unexpected symbol '" + c + "' at " + (start + segment.position() - 1));
            }
        }
        stats.tock(System.nanoTime());
        return this;
    }

    public long[] counts() {
        return new long[] {
                counta, countc, countg, countt,
        };
    }


    public Statistics stats() {
        return stats;
    }

}
