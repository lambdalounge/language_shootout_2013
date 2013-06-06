package kata.stats;

import java.util.concurrent.TimeUnit;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Tests for a samples container allowing basic statistical calculations. The container uses O(1) memory.
 * Based on http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
 *
 * @author Iliyan Jeliazkov
 *
 */
public class StatisticsTest {

    @Test
    public void testToString() {
        Statistics stats = new Statistics();
        assertEquals("NaN (stdev=0.00, min=9223372036854775807, max=-9223372036854775808)", stats.toString());
        stats.sample(1000);
        assertEquals("1000.00 (stdev=0.00, min=1000, max=1000)", stats.toString());
        stats.sample(2000);
        assertEquals("1500.00 (stdev=707.11, min=1000, max=2000)", stats.toString());
    }

    @Test
    public void testToStringDefaultConversions() {
        Statistics stats = new Statistics(0, "ns");
        assertEquals("NaNns (stdev=0.00ns, min=9223372036854775807ns, max=-9223372036854775808ns)", stats.toString());

        stats.tock(1000000000L);
        assertEquals("1000000000.00ns (stdev=0.00ns, min=1000000000ns, max=1000000000ns)", stats.toString());

        stats.tock(3000000000L);
        assertEquals("1500000000.00ns (stdev=707106781.19ns, min=1000000000ns, max=2000000000ns)", stats.toString());
    }

    @Test
    public void testToStringConversions() {
        Statistics stats = new Statistics(0, "ms", TimeUnit.MILLISECONDS.toNanos(1));
        assertEquals("NaNms (stdev=0.00ms, min=9223372036854.78ms, max=-9223372036854.78ms)", stats.toString());

        stats.tock(1000000000L);
        assertEquals("1000.00ms (stdev=0.00ms, min=1000.00ms, max=1000.00ms)", stats.toString());

        stats.tock(3000000000L);
        assertEquals("1500.00ms (stdev=707.11ms, min=1000.00ms, max=2000.00ms)", stats.toString());
    }

    @Test
    public void testFormattingDefaults() {
        Statistics stats = new Statistics("ms");
        assertEquals("NaNms (stdev=0.000000ms, min=9223372036854775807ms, max=-9223372036854775808ms)", String.format("%s", stats));
        stats.sample(1000);
        assertEquals("1000.000000ms (stdev=0.000000ms, min=1000ms, max=1000ms)", String.format("%s", stats));
        stats.sample(2000);
        assertEquals("1500.000000ms (stdev=707.106781ms, min=1000ms, max=2000ms)", String.format("%s", stats));
    }

    @Test
    public void testFormattingSpecificPrecision() {
        Statistics stats = new Statistics("ms");
        assertEquals("NaNms (stdev=0.00000000ms, min=9223372036854775807ms, max=-9223372036854775808ms)", String.format("%.8s", stats));
        stats.sample(1000);
        assertEquals("1000.00000000ms (stdev=0.00000000ms, min=1000ms, max=1000ms)", String.format("%.8s", stats));
        stats.sample(2000);
        assertEquals("1500.00000000ms (stdev=707.10678119ms, min=1000ms, max=2000ms)", String.format("%.8s", stats));
    }

    @Test
    public void testTickTock() throws Exception {
        Statistics stats = new Statistics(System.nanoTime(), "ms");
        Thread.sleep(500);
        long newbeginning = System.nanoTime();
        stats.start(newbeginning);
        stats.tock(1000+newbeginning);

        assertEquals(1000.0, stats.median(), 1.0e-6);
        assertEquals(0.0, stats.deviation(), 1.0e-6);
        assertEquals(0.0, stats.variance(), 1.0e-6);
        assertEquals(1000, stats.minimum());
        assertEquals(1000, stats.maximum());
    }

    @Test
    public void testDefault() {
        Statistics stats = new Statistics();
        assertEquals(0.0, stats.deviation(), 1.0e-6);
        assertEquals(0.0, stats.variance(), 1.0e-6);
        assertEquals(Long.MAX_VALUE, stats.minimum());
        assertEquals(Long.MIN_VALUE, stats.maximum());
    }

    @Test
    public void testSingleSample() {
        Statistics stats = new Statistics();
        stats.sample(1000);
        assertEquals(1000.0, stats.median(), 1.0e-6);
        assertEquals(0.0, stats.deviation(), 1.0e-6);
        assertEquals(0.0, stats.variance(), 1.0e-6);
        assertEquals(1000, stats.minimum());
        assertEquals(1000, stats.maximum());
    }

    @Test
    public void testTwoSamples() {
        Statistics stats = new Statistics();
        stats.sample(2000);
        stats.sample(1000);
        assertEquals(1500.0, stats.median(), 1.0e-6);
        assertEquals(707.106781, stats.deviation(), 1.0e-6);
        assertEquals(500000.0, stats.variance(), 1.0e-6);
        assertEquals(1000, stats.minimum());
        assertEquals(2000, stats.maximum());
    }

}
