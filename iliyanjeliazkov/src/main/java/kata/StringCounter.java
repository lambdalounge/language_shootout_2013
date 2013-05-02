package kata;

import java.nio.CharBuffer;

public class StringCounter implements Screener {

    private final Statistics stats = new Statistics("ms");

    int counta;
    int countc;
    int countg;
    int countt;

    public void process(CharBuffer b) {
        stats.start(System.nanoTime());
        char c = b.get();
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
                throw new IllegalArgumentException("Unexpected symbol '" + c + "' at " + (b.position() - 1));
        }
        stats.tock(System.nanoTime());
    }

    public Statistics stats() {
        return stats;
    }
}
