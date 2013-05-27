package shootout;

import org.junit.Test;

import static org.fest.assertions.Assertions.assertThat;

public class CounterTest {
  @Test public void countsEmptyString() {
    assertThat(new Counter().count("")).isEqualTo("0 0 0 0");
  }

  @Test public void countsExampleFromWebSite() {
    assertThat(new Counter().count(
        "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")).isEqualTo(
        "20 12 17 21");
  }
}
