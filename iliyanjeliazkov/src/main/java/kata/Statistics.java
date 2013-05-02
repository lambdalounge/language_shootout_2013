package kata;

import java.util.Formattable;
import java.util.Formatter;

/**
 * A samples container allowing basic statistical calculations. The container uses O(1) memory.
 * Based on http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
 *
 * @author Iliyan Jeliazkov
 *
 */
public class Statistics implements Formattable {

    private long maximum = Long.MIN_VALUE;
    private long minimum = Long.MAX_VALUE;
    private final String units;
    private long count;
    private double total;
    private double mean;
    private double mean2;
    private long tick;
    private final long scale;

    public Statistics(long tick, String units, long scale) {
        this.tick = tick;
        this.units = units;
        assert (scale > 0.0);
        this.scale = scale;
    }

    public Statistics(long start, String units) {
        this(start, units, 1);
    }

    public Statistics(String units) {
        this(0, units);
    }

    public Statistics() {
        this(0, "");
    }

    /**
     * Adjust the stamp for the last tick. Useful when necessary to "rebase" this instance by
     * eliminating a known interval of time that may have elapsed between the instance creation
     * and the first call to {@link #tock(long)}. For example, a {@link Statistics} may be
     * created just before a lengthly operation, which precedes the iteration that generates the
     * samples. Thus without "resetting" before the first sample it would have had an out-sized
     * bias.
     */
    public void start(long tick) {
        this.tick = tick;
    }

    /**
     * Add new sample whose absolute value equals the difference between {@link #tick} and
     * {@link #tock}
     */
    public void tock(long tock) {
        assert (tock >= tick);
        sample(tock - tick);
        tick = tock;
    }

    /** Add new sample */
    public void sample(long sample) {

        maximum = Math.max(maximum, sample);
        minimum = Math.min(minimum, sample);
        total += sample;

        count++;

        double delta = sample - mean;
        mean += delta / count;
        mean2 += delta * (sample - mean);
    }

    public long minimum() {
        return minimum;
    }

    public long maximum() {
        return maximum;
    }

    public long count() {
        return count;
    }

    /** Calculates the "sample" variation. [O(1)] */
    public double variance() {

        if (count < 2) {
            return 0;
        }

        return mean2 / (count - 1);
    }

    /** Calculates the sample standard deviation. [O(1)] */
    public double deviation() {
        return Math.sqrt(variance());
    }

    /** Calculates the median. [O(1)] */
    public double median() {
        if (count == 0) {
            return Double.NaN;
        }

        return mean;
    }

    public String toString(int precision) {
        Formatter formatter = new Formatter();
        this.formatTo(formatter, 0, 1, precision);
        return formatter.toString();
    }

    @Override
    public String toString() {
        return toString(2);
    }

    public double total() {
        return total;
    }

    public double scale() {
        return scale;
    }

    public String units() {
        return units;
    }

    public void formatTo(Formatter formatter,
                         int flags,
                         int width,
                         int precision) {

        StringBuilder fmt = new StringBuilder();
        appendDouble(fmt, width, precision);

        fmt.append(" (stdev=");
        appendDouble(fmt, width, precision);

        fmt.append(", min=");
        if (scale == 1) {
            fmt.append("%d");
            fmt.append(units);
        } else {
            appendDouble(fmt, width, precision);
        }

        fmt.append(", max=");
        if (scale == 1) {
            fmt.append("%d");
            fmt.append(units);
        } else {
            appendDouble(fmt, width, precision);
        }

        fmt.append(")");

        if (scale == 1) {
            formatter.format(fmt.toString(), median(), deviation(), minimum(), maximum());
        } else {
            formatter.format(fmt.toString(),
                             median() / scale,
                             deviation() / scale,
                             (double) minimum() / scale,
                             (double) maximum() / scale);
        }
    }

    StringBuilder appendDouble(StringBuilder fmt,
                             int width,
                             int precision) {
        fmt.append("%");
        if (width > 0) {
            fmt.append(width);
        }
        if (precision > 0) {
            fmt.append(".");
            fmt.append(precision);
        }
        fmt.append("f");
        fmt.append(units);
        return fmt;
    }

}
