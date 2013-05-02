package kata;

import java.nio.CharBuffer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import kata.stats.Statistics;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.fail;

public class StringCounterTest {

    private static final int MAX_REPETITIONS = 10;
    private static final int MAX_CONCURRENCY = 8;
    private final String shortSample = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";

    @Test
    public void testBadSample() throws Exception {
        Screener counter = new Screener();
        try {
            counter.process(CharBuffer.wrap("Q"));
            fail("Expected to fail");
        } catch (IllegalArgumentException ex) {
            ;
        }

        Assert.assertArrayEquals(new long[] {
                0, 0, 0, 0
        }, counter.counts());
    }

    @Test
    public void testSingleSample() throws Exception {
        Screener counter = new Screener();
        counter.process(createShortSample());
        Assert.assertArrayEquals(new long[] {
                20, 12, 17, 21
        }, counter.counts());
    }

//    @Test
//    public void testSingleSampleWithStrategyNoRepetition() throws Exception {
//        Strategy strategy = new SequentialProcessing();
//        Statistics counter = strategy.process(1, createShortSample());
//        Assert.assertArrayEquals(new long[] {
//                20, 12, 17, 21
//        }, counter.counts());
//    }
//
//    @Test
//    public void testSingleSampleWithStrategyTwice() throws Exception {
//        Strategy strategy = new SequentialProcessing();
//        Statistics counter = strategy.process(2, createShortSample());
//        Assert.assertArrayEquals(new long[] {
//                40, 24, 34, 42
//        }, counter.counts());
//    }

    @Test
    public void testManyLongSamplesWithParallelStrategy() throws Exception {
        CharBuffer wraped = createLongSample();
        int length = wraped.remaining();
        ExecutorService pool = Executors.newFixedThreadPool(MAX_CONCURRENCY);
        Strategy strategy = new ParallelProcessing(MAX_CONCURRENCY, pool);
        for (long repetitions = 1; repetitions <= MAX_REPETITIONS; repetitions *= 10) {
            Statistics stats = strategy.process(repetitions, wraped);
            System.out.printf("%npar long(%d, %d) %.2s, total: %.2fms", length, repetitions, stats, stats.total() / 1000000);
        }
        pool.shutdown();
    }

    @Test
    public void testShortSamplesWithParallelStrategy() throws Exception {
        CharBuffer wraped = createShortSample();
        int length = wraped.remaining();
        ExecutorService pool = Executors.newFixedThreadPool(MAX_CONCURRENCY);
        Strategy strategy = new ParallelProcessing(MAX_CONCURRENCY, pool);
        for (long repetitions = 1; repetitions <= MAX_REPETITIONS; repetitions *= 10) {
            Statistics stats = strategy.process(repetitions, wraped);
            System.out.printf("%npar short(%d, %d) %.2s, total: %.2fms", length, repetitions, stats, stats.total() / 1000000);
        }
        pool.shutdown();
    }


    @Test
    public void testLongSamplesWithSequentialStrategy() throws Exception {
        CharBuffer sequence = createLongSample();
        int length = sequence.remaining();
        Strategy strategy = new SequentialProcessing();
        for (long n = 1; n <= MAX_REPETITIONS; n *= 10) {
            Statistics stats = strategy.process(n, sequence);
            System.out.printf("%nseq long(%d, %d) %.2s, total: %.2fms", length, n, stats, stats.total() / 1000000);
        }
    }

    @Test
    public void testShortSamplesWithSequentialStrategy() throws Exception {
        CharBuffer sequence = createShortSample();
        int length = sequence.remaining();
        Strategy strategy = new SequentialProcessing();
        for (long n = 1; n <= MAX_REPETITIONS; n *= 10) {
            Statistics stats = strategy.process(n, sequence);
            System.out.printf("%nseq short(%d, %d) %.2s, total: %.2fms", length, n, stats, stats.total() / 1000000);
        }
    }

    public CharBuffer createShortSample() {
        return CharBuffer.wrap(shortSample);
    }

    public CharBuffer createLongSample() {
        CharBuffer buffer = CharBuffer.allocate(Integer.MAX_VALUE / 4);
        while (buffer.hasRemaining()) {
            buffer.put(shortSample, 0, Math.min(buffer.remaining(), shortSample.length()));
        }
        buffer.flip();
        return buffer;
    }

}
