package kata;

import java.nio.CharBuffer;

import kata.stats.Statistics;

public interface Strategy {

    public abstract Statistics process(long repetitions,
                                     CharBuffer sequence);

}
