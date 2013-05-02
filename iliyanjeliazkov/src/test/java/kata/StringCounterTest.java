package kata;

import java.nio.CharBuffer;

import org.junit.Test;

public class StringCounterTest {

    private final String sample = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";

    @Test
    public void testOnce() throws Exception {
        Screener counter = new StringCounter();
        counter.process(CharBuffer.wrap(sample));
        System.out.printf("once: %s", counter.stats());
    }

}
