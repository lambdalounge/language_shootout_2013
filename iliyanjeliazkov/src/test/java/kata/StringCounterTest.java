package kata;

import java.nio.CharBuffer;

import org.junit.Assert;
import org.junit.Test;

public class StringCounterTest {

    private final String shortSample = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";

    @Test
    public void testSample() throws Exception {
        Screener counter = new StringCounter();
        counter.process(createShortSample());
        Assert.assertArrayEquals(new long[] {
                20, 12, 17, 21
        }, counter.counts());
    }

    @Test
    public void testManyLongSamplesSequential() throws Exception {
        Screener counter = new StringCounter();
        CharBuffer wraped = createLongSample();
        for (long n = 1; n <= 100000000; n *= 10) {
            for (long i = 0; i < n; i++) {
                counter.process((CharBuffer) wraped.rewind());
            }

            Statistics stats = counter.stats();
            System.out.printf("%nlong(%d) %.2s, total: %.2fms", n, stats, stats.total() / 1000000);
        }
        Assert.assertArrayEquals(new long[] {
                2222222220L, 1333333332L, 188870000, 233310000
        }, counter.counts());
    }

    @Test
    public void testManyShortSamples() throws Exception {
        Screener counter = new StringCounter();
        CharBuffer wraped = createShortSample();
        for (long n = 1; n <= 100000000; n *= 10) {
            for (long i = 0; i < n; i++) {
                counter.process((CharBuffer) wraped.rewind());
            }

            Statistics stats = counter.stats();
            System.out.printf("%nshort(%d) %.2s, total: %.2fms", n, stats, stats.total() / 1000000);
        }
        Assert.assertArrayEquals(new long[] {
                2222222220L, 1333333332L, 188870000, 233310000
        }, counter.counts());
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
