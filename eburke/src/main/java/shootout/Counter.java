package shootout;

/**
 * "Counting DNA Nucleotides" as described at http://rosalind.info/problems/dna/.
 *
 * @author Eric Burke
 */
public class Counter {
  public static void main(String... args) {
    if (args.length > 0) {
      System.out.println(new Counter().count(args[0]));
    }
  }

  /**
   * Implemented as a Counting Sort, allocating an array of length 85 to hold counts at indices
   * corresponding to ASCII codes. This could also be implemented as an array of length 4, but
   * you'd have to use a switch statement or subtraction to determine the array index for each
   * character.
   */
  public String count(String dna) {
    int[] acgt = new int['T' + 1];
    for (char symbol : dna.toCharArray()) acgt[symbol]++;
    return String.format("%d %d %d %d", acgt['A'], acgt['C'], acgt['G'], acgt['T']);
  }
}